//! CEL parser: recursive descent over the token stream, producing an
//! arena-allocated AST (see ast.zig).
//!
//! Semantics follow cel-spec/doc/langdef.md and the reference implementations
//! (cel-go / cel-cpp):
//!
//! - operators become calls with internal names (`_+_`, `_[_]`, `@in`, ...);
//! - `&&`/`||`/`!`/ternary keep dedicated nodes for their special evaluation
//!   semantics;
//! - macros (`has`, `all`, `exists`, `exists_one`, `filter`, `map`) are
//!   recognized syntactically and expanded to macro nodes (or, for `has()`,
//!   selects flagged `test_only`); calls that merely *look* like macros but
//!   have the wrong shape remain ordinary calls and fail at evaluation;
//! - `-<int literal>` is folded to a constant, including the min-int case
//!   `-9223372036854775808` (the magnitude alone exceeds maxInt(i64));
//! - reserved words are rejected wherever identifiers are required, except
//!   in receiver-call position (`a.package()`), which the spec allows.
//!
//! Macro node layout: children are [iter_var (ident node), range,
//! predicate, transform?] and `payload.macro` selects the macro kind.

const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const ext = @import("ext.zig");

pub const Ast = ast.Ast;
pub const Node = ast.Node;
pub const NodeIndex = ast.NodeIndex;
pub const MacroKind = ast.MacroKind;

pub const Error = error{ Parse, Lex, OutOfMemory };

pub const Options = struct {
    /// Parse calls that syntactically match macros as ordinary function
    /// calls (conformance: `disable_macros`).
    disable_macros: bool = false,
    /// Enabled CEL extensions (opt-in, like cel-go's ext.Math()/ext.Strings()):
    /// `math` enables the math.greatest/math.least macros and the
    /// math.<fn>(...) qualified function calls; `strings` enables
    /// strings.quote(...).
    ext: ext.Extensions = .{},
};

/// Max expression nesting depth (defensive bound; cel-go uses 250).
const max_depth: usize = 250;

/// Internal operator function names, following cel-go's naming scheme.
pub const ops = struct {
    pub const index = "_[_]";
    pub const add = "_+_";
    pub const sub = "_-_";
    pub const mul = "_*_";
    pub const div = "_/_";
    pub const rem = "_%_";
    pub const neg = "-_";
    pub const lt = "_<_";
    pub const le = "_<=_";
    pub const gt = "_>_";
    pub const ge = "_>=_";
    pub const eq = "_==_";
    pub const ne = "_!=_";
    pub const in_ = "@in";
};

/// The result of a top-level parse: either an AST or a diagnostic.
pub const Parsed = union(enum) {
    ok: Ast,
    err: struct { msg: []const u8, pos: usize },
};

/// Parse `src`, reporting any diagnostic in the returned value.
pub fn parseSrc(alloc: std.mem.Allocator, src: []const u8, options: Options) std.mem.Allocator.Error!Parsed {
    var lex = lexer.Lexer.init(alloc, src);
    var toks = std.ArrayList(token.Token){};
    defer toks.deinit(alloc);
    while (true) {
        const t = lex.next() catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Lex => return .{ .err = .{ .msg = try alloc.dupe(u8, lex.err_msg), .pos = lex.err_pos } },
        };
        const done = t.kind == .eof;
        try toks.append(alloc, t);
        if (done) break;
    }
    var p = Parser.init(alloc, try toks.toOwnedSlice(alloc), options);
    const root = p.parseRoot() catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return .{ .err = .{ .msg = try alloc.dupe(u8, p.err_msg), .pos = p.err_pos } },
    };
    return .{ .ok = root };
}

pub const Parser = struct {
    alloc: std.mem.Allocator,
    toks: []const token.Token,
    pos: usize = 0,
    nodes: std.ArrayList(ast.Node),
    child_table: std.ArrayList(NodeIndex),
    err_msg: []const u8 = "",
    err_pos: usize = 0,
    depth: usize = 0,
    disable_macros: bool = false,
    options: Options = .{},

    const P = Parser;

    pub fn init(alloc: std.mem.Allocator, toks: []const token.Token, options: Options) P {
        return .{
            .alloc = alloc,
            .toks = toks,
            .nodes = .empty,
            .child_table = .empty,
            .disable_macros = options.disable_macros,
            .options = options,
        };
    }

    fn parseRoot(p: *P) Error!Ast {
        if (p.toks.len == 0 or p.toks[0].kind == .eof) return p.fail(0, "empty expression");
        const root = try p.parseExpr();
        const t = p.cur();
        if (t.kind != .eof) return p.fail(t.start, "unexpected trailing input");
        return .{
            .nodes = try p.nodes.toOwnedSlice(p.alloc),
            .child_table = try p.child_table.toOwnedSlice(p.alloc),
            .root = root,
        };
    }

    fn fail(p: *P, pos: usize, msg: []const u8) Error {
        p.err_msg = msg;
        p.err_pos = pos;
        return error.Parse;
    }

    fn failFmt(p: *P, pos: usize, comptime fmt: []const u8, args: anytype) Error {
        p.err_msg = std.fmt.allocPrint(p.alloc, fmt, args) catch "out of memory";
        p.err_pos = pos;
        return error.Parse;
    }

    fn cur(p: *P) token.Token {
        return p.toks[p.pos];
    }

    fn peek(p: *P, offset: usize) token.Token {
        const i = p.pos + offset;
        return if (i < p.toks.len) p.toks[i] else p.toks[p.toks.len - 1];
    }

    fn advance(p: *P) void {
        if (p.pos + 1 < p.toks.len) p.pos += 1;
    }

    /// Byte offset one past the last consumed token. The parser never
    /// advances past the eof token, so `pos - 1` is always the most
    /// recently consumed real token.
    fn prevEnd(p: *P) u32 {
        return p.toks[p.pos - 1].end;
    }

    fn expect(p: *P, kind: token.TokenKind, what: []const u8) Error!token.Token {
        const t = p.cur();
        if (t.kind != kind) {
            return p.fail(t.start, what);
        }
        p.advance();
        return t;
    }

    fn childOf(p: *P, i: NodeIndex) NodeIndex {
        const n = p.nodes.items[i];
        return p.child_table.items[n.child];
    }

    // ---- node construction helpers ----

    fn addNode(p: *P, n: ast.Node) Error!NodeIndex {
        try p.nodes.append(p.alloc, n);
        return @intCast(p.nodes.items.len - 1);
    }

    fn setChildren(p: *P, n: NodeIndex, kids: []const NodeIndex) Error!void {
        const start = p.child_table.items.len;
        try p.child_table.appendSlice(p.alloc, kids);
        p.nodes.items[n].child = @intCast(start);
        p.nodes.items[n].nchild = @intCast(kids.len);
    }

    fn binary(p: *P, kind: ast.Kind, lhs: NodeIndex, rhs: NodeIndex, start: u32) Error!NodeIndex {
        const n = try p.addNode(.{ .kind = kind, .start = start, .end = p.prevEnd() });
        try p.setChildren(n, &.{ lhs, rhs });
        return n;
    }

    fn call(p: *P, name: []const u8, args: []const NodeIndex, start: u32, receiver: bool) Error!NodeIndex {
        const n = try p.addNode(.{ .kind = .call, .start = start, .end = p.prevEnd(), .payload = .{ .str = name } });
        if (receiver) p.nodes.items[n].flags.receiver = true;
        try p.setChildren(n, args);
        return n;
    }

    // ---- grammar ----

    fn parseExpr(p: *P) Error!NodeIndex {
        const start = p.cur().start;
        const cond = try p.parseOr();
        if (p.cur().kind == .question) {
            p.advance();
            const then = try p.parseOr();
            _ = try p.expect(.colon, "expected ':' in conditional expression");
            const els = try p.parseExpr();
            const n = try p.addNode(.{ .kind = .ternary, .start = start, .end = p.prevEnd() });
            try p.setChildren(n, &.{ cond, then, els });
            return n;
        }
        return cond;
    }

    fn parseOr(p: *P) Error!NodeIndex {
        var lhs = try p.parseAnd();
        while (p.cur().kind == .or_op) {
            const start = p.nodes.items[lhs].start;
            p.advance();
            const rhs = try p.parseAnd();
            lhs = try p.binary(.or_, lhs, rhs, start);
        }
        return lhs;
    }

    fn parseAnd(p: *P) Error!NodeIndex {
        var lhs = try p.parseRelation();
        while (p.cur().kind == .and_op) {
            const start = p.nodes.items[lhs].start;
            p.advance();
            const rhs = try p.parseRelation();
            lhs = try p.binary(.and_, lhs, rhs, start);
        }
        return lhs;
    }

    fn parseRelation(p: *P) Error!NodeIndex {
        var lhs = try p.parseAddition();
        while (true) {
            const t = p.cur();
            const name: []const u8 = switch (t.kind) {
                .lt => ops.lt,
                .le => ops.le,
                .gt => ops.gt,
                .ge => ops.ge,
                .eq => ops.eq,
                .ne => ops.ne,
                .in_kw => ops.in_,
                else => break,
            };
            p.advance();
            const start = p.nodes.items[lhs].start;
            const rhs = try p.parseAddition();
            lhs = try p.call(name, &.{ lhs, rhs }, start, false);
        }
        return lhs;
    }

    fn parseAddition(p: *P) Error!NodeIndex {
        var lhs = try p.parseMultiplication();
        while (true) {
            const t = p.cur();
            const name: []const u8 = switch (t.kind) {
                .plus => ops.add,
                .minus => "_-_",
                else => break,
            };
            p.advance();
            const start = p.nodes.items[lhs].start;
            const rhs = try p.parseMultiplication();
            lhs = try p.call(name, &.{ lhs, rhs }, start, false);
        }
        return lhs;
    }

    fn parseMultiplication(p: *P) Error!NodeIndex {
        var lhs = try p.parseUnary();
        while (true) {
            const t = p.cur();
            const name: []const u8 = switch (t.kind) {
                .star => ops.mul,
                .slash => ops.div,
                .percent => ops.rem,
                else => break,
            };
            p.advance();
            const start = p.nodes.items[lhs].start;
            const rhs = try p.parseUnary();
            lhs = try p.call(name, &.{ lhs, rhs }, start, false);
        }
        return lhs;
    }

    fn parseUnary(p: *P) Error!NodeIndex {
        const t = p.cur();
        switch (t.kind) {
            .not => {
                p.advance();
                const operand = try p.parseUnary();
                // Fold double negation; the surviving child's span is
                // widened to cover both `!`s.
                if (p.nodes.items[operand].kind == .not) {
                    const child = p.childOf(operand);
                    p.nodes.items[child].start = t.start;
                    return child;
                }
                const n = try p.addNode(.{ .kind = .not, .start = t.start, .end = p.prevEnd() });
                try p.setChildren(n, &.{operand});
                return n;
            },
            .minus => {
                p.advance();
                // Fold negative int literals (including min int).
                const nt = p.cur();
                if (nt.kind == .int_lit) {
                    if (nt.overflow and nt.num.uint == @as(u64, 1) << 63) {
                        p.advance();
                        return p.addNode(.{ .kind = .int, .start = t.start, .end = nt.end, .payload = .{ .int = std.math.minInt(i64) } });
                    }
                    if (!nt.overflow) {
                        p.advance();
                        return p.addNode(.{ .kind = .int, .start = t.start, .end = nt.end, .payload = .{ .int = -nt.num.int } });
                    }
                }
                const operand = try p.parseUnary();
                if (p.nodes.items[operand].kind == .neg) {
                    return p.childOf(operand);
                }
                // Fold negation of an int literal (e.g. `--5` -> 5); the
                // literal's span is widened to cover both `-`s.
                if (p.nodes.items[operand].kind == .int) {
                    const v = p.nodes.items[operand].payload.int;
                    if (v != std.math.minInt(i64)) {
                        p.nodes.items[operand].payload.int = -v;
                        p.nodes.items[operand].start = t.start;
                        return operand;
                    }
                }
                const n = try p.addNode(.{ .kind = .neg, .start = t.start, .end = p.prevEnd() });
                try p.setChildren(n, &.{operand});
                return n;
            },
            else => return p.parseMember(),
        }
    }

    fn parseMember(p: *P) Error!NodeIndex {
        var e = try p.parsePrimary();
        while (true) {
            const t = p.cur();
            switch (t.kind) {
                .dot => {
                    const ft = p.peek(1);
                    if (ft.kind != .ident) {
                        if (ft.kind == .int_lit or ft.kind == .double_lit or ft.kind == .uint_lit) {
                            return p.fail(t.start, "invalid character after '.'");
                        }
                        return p.fail(ft.start, "expected identifier after '.'");
                    }
                    p.advance(); // '.'
                    p.advance(); // ident
                    // Receiver call: e.f(args...). Reserved words are allowed
                    // in this position per the spec.
                    if (p.cur().kind == .lparen) {
                        const args = try p.parseArgList();
                        const recv_start = p.nodes.items[e].start;
                        if (try p.maybeMacro(e, ft, args, recv_start)) |m| {
                            e = m;
                        } else if (p.qualifiedFunctionName(e, ft.text)) |dotted| {
                            // Namespaced extension function (math.ceil,
                            // strings.quote): a global call with the dotted
                            // name, mirroring cel-go's checker resolution.
                            e = try p.call(dotted, args, recv_start, false);
                        } else {
                            // The receiver becomes the first argument.
                            const kids = try p.alloc.alloc(NodeIndex, args.len + 1);
                            kids[0] = e;
                            @memcpy(kids[1..], args);
                            e = try p.call(ft.text, kids, recv_start, true);
                        }
                        continue;
                    }
                    const sel = try p.addNode(.{ .kind = .select, .start = p.nodes.items[e].start, .end = ft.end, .payload = .{ .str = ft.text } });
                    try p.setChildren(sel, &.{e});
                    e = sel;
                },
                .lbracket => {
                    p.advance();
                    const idx = try p.parseExpr();
                    _ = try p.expect(.rbracket, "expected ']' to close index expression");
                    e = try p.call(ops.index, &.{ e, idx }, p.nodes.items[e].start, false);
                },
                else => break,
            }
        }
        return e;
    }

    /// Parse a parenthesized argument list; `(` is the current token.
    fn parseArgList(p: *P) Error![]NodeIndex {
        _ = try p.expect(.lparen, "expected '('");
        var args = std.ArrayList(NodeIndex){};
        defer args.deinit(p.alloc);
        if (p.cur().kind != .rparen) {
            while (true) {
                try args.append(p.alloc, try p.parseExpr());
                if (p.cur().kind == .comma) {
                    p.advance();
                    if (p.cur().kind == .rparen) break; // trailing comma
                    continue;
                }
                break;
            }
        }
        _ = try p.expect(.rparen, "expected ')' to close argument list");
        return args.toOwnedSlice(p.alloc);
    }

    /// The math extension function names callable as `math.<name>(...)`.
    const math_fns = [_][]const u8{
        "@min", "@max", "ceil",   "floor", "round", "trunc", "abs",
        "sign", "isInf", "isNaN", "isFinite", "sqrt", "bitAnd", "bitOr",
        "bitXor", "bitNot", "bitShiftLeft", "bitShiftRight",
    };

    /// Whether a receiver call `target.name(args)` should be rewritten as a
    /// global call to the dotted function name (e.g. `math.ceil(x)` ->
    /// `math.ceil(x)` as a global function, mirroring cel-go's checker
    /// resolution of namespaced functions). Returns the dotted name.
    fn qualifiedFunctionName(p: *P, target: NodeIndex, name: []const u8) ?[]const u8 {
        const t = p.nodes.items[target];
        if (t.kind != .ident or t.flags.abs) return null;
        if (std.mem.eql(u8, t.payload.str, "math") and p.options.ext.math) {
            for (math_fns) |fn_name| {
                if (std.mem.eql(u8, fn_name, name)) {
                    return std.fmt.allocPrint(p.alloc, "math.{s}", .{name}) catch null;
                }
            }
        }
        if (std.mem.eql(u8, t.payload.str, "strings") and p.options.ext.strings) {
            if (std.mem.eql(u8, name, "quote")) {
                return "strings.quote";
            }
        }
        return null;
    }

    /// If `target.name(args)` matches a comprehension macro shape, expand it.
    /// Returns null when the call is not macro-shaped (it remains a call and
    /// fails at evaluation as an unknown function).
    fn maybeMacro(p: *P, target: NodeIndex, name_tok: token.Token, args: []const NodeIndex, start: u32) Error!?NodeIndex {
        if (p.disable_macros) return null;
        const name = name_tok.text;
        // Math extension macros: math.greatest(...) / math.least(...).
        if (p.options.ext.math) {
            const t = p.nodes.items[target];
            if (t.kind == .ident and !t.flags.abs and std.mem.eql(u8, t.payload.str, "math") and
                (std.mem.eql(u8, name, "greatest") or std.mem.eql(u8, name, "least")))
            {
                return try p.expandMathMacro(target, name, args, start);
            }
        }
        var mkind: ?MacroKind = null;
        if (std.mem.eql(u8, name, "all")) {
            mkind = .all;
        } else if (std.mem.eql(u8, name, "exists")) {
            mkind = .exists;
        } else if (std.mem.eql(u8, name, "exists_one")) {
            mkind = .exists_one;
        } else if (std.mem.eql(u8, name, "filter")) {
            mkind = .filter;
        } else if (std.mem.eql(u8, name, "map")) {
            mkind = if (args.len == 3) .map_filter else .map;
        } else return null;

        if (mkind == .map and args.len != 2) return null;
        if (mkind == .map_filter and args.len != 3) return null;
        if (mkind != .map and mkind != .map_filter and args.len != 2) return null;

        // The iteration variable must be a simple identifier.
        if (args.len == 0) return null;
        const var_node = p.nodes.items[args[0]];
        if (var_node.kind != .ident) return null;
        if (var_node.flags.abs) return null;

        const m = mkind.?;
        const n = try p.addNode(.{ .kind = .macro, .start = start, .end = p.prevEnd(), .payload = .{ .macro = m } });
        var kids = std.ArrayList(NodeIndex){};
        defer kids.deinit(p.alloc);
        try kids.append(p.alloc, args[0]); // iteration variable (ident node)
        try kids.append(p.alloc, target); // range
        try kids.appendSlice(p.alloc, args[1..]); // predicate [+ transform]
        try p.setChildren(n, kids.items);
        return n;
    }

    /// Expand `math.greatest(args...)` / `math.least(args...)` into a
    /// `math.@max` / `math.@min` call (cel-go ext/math.go):
    ///   1 numeric-or-dynamic arg  -> math.@min(arg)
    ///   2 args                    -> math.@min(a, b)
    ///   3+ args                   -> math.@min([a, b, c])
    fn expandMathMacro(p: *P, target: NodeIndex, name: []const u8, args: []const NodeIndex, start: u32) Error!NodeIndex {
        _ = target;
        const min_form = std.mem.eql(u8, name, "least");
        const fn_name: []const u8 = if (min_form) "math.@min" else "math.@max";
        const macro_name: []const u8 = if (min_form) "math.least()" else "math.greatest()";
        switch (args.len) {
            0 => return p.failFmt(start, "{s} requires at least one argument", .{macro_name}),
            1 => {
                if (isNumericArg(p, args[0]) or isNumericListLiteral(p, args[0])) {
                    return p.call(fn_name, args, start, false);
                }
                return p.failFmt(start, "{s} invalid single argument value", .{macro_name});
            },
            2 => {
                try p.checkNumericArgs(macro_name, args);
                return p.call(fn_name, args, start, false);
            },
            else => {
                try p.checkNumericArgs(macro_name, args);
                const list = try p.addNode(.{ .kind = .list, .start = start, .end = p.prevEnd() });
                try p.setChildren(list, args);
                return p.call(fn_name, &.{list}, start, false);
            },
        }
    }

    /// cel-go's isNumericArgType: literals must be numeric; list/map/struct
    /// and other literals are not; non-literals (idents, calls) are
    /// acceptable.
    fn isNumericArg(p: *P, node: NodeIndex) bool {
        const n = p.nodes.items[node];
        return switch (n.kind) {
            .int, .uint, .double => true,
            .string, .bytes, .bool_, .null_, .list, .map, .struct_ => false,
            else => true,
        };
    }

    /// cel-go's isListLiteralWithNumericArgs: a non-empty list literal whose
    /// elements are all numeric args.
    fn isNumericListLiteral(p: *P, node: NodeIndex) bool {
        const n = p.nodes.items[node];
        if (n.kind != .list) return false;
        const kids = n.children(p.child_table.items);
        if (kids.len == 0) return false;
        for (kids) |k| {
            if (!p.isNumericArg(k)) return false;
        }
        return true;
    }

    fn checkNumericArgs(p: *P, macro_name: []const u8, args: []const NodeIndex) Error!void {
        for (args) |arg| {
            if (!p.isNumericArg(arg)) {
                return p.failFmt(p.nodes.items[arg].start, "{s} simple literal arguments must be numeric", .{macro_name});
            }
        }
    }

    fn parsePrimary(p: *P) Error!NodeIndex {
        p.depth += 1;
        defer p.depth -= 1;
        if (p.depth > max_depth) return p.fail(p.cur().start, "expression nesting too deep");

        const t = p.cur();
        switch (t.kind) {
            .int_lit => {
                p.advance();
                if (t.overflow) return p.fail(t.start, "int literal out of range");
                return p.addNode(.{ .kind = .int, .start = t.start, .end = t.end, .payload = .{ .int = t.num.int } });
            },
            .uint_lit => {
                p.advance();
                return p.addNode(.{ .kind = .uint, .start = t.start, .end = t.end, .payload = .{ .uint = t.num.uint } });
            },
            .double_lit => {
                p.advance();
                return p.addNode(.{ .kind = .double, .start = t.start, .end = t.end, .payload = .{ .double = t.num.double } });
            },
            .string_lit => {
                p.advance();
                return p.addNode(.{ .kind = .string, .start = t.start, .end = t.end, .payload = .{ .str = t.str } });
            },
            .bytes_lit => {
                p.advance();
                return p.addNode(.{ .kind = .bytes, .start = t.start, .end = t.end, .payload = .{ .str = t.str } });
            },
            .true_kw => {
                p.advance();
                return p.addNode(.{ .kind = .bool_, .start = t.start, .end = t.end, .payload = .{ .int = 1 } });
            },
            .false_kw => {
                p.advance();
                return p.addNode(.{ .kind = .bool_, .start = t.start, .end = t.end, .payload = .{ .int = 0 } });
            },
            .null_kw => {
                p.advance();
                return p.addNode(.{ .kind = .null_, .start = t.start, .end = t.end });
            },
            .lparen => {
                p.advance();
                const e = try p.parseExpr();
                _ = try p.expect(.rparen, "expected ')' to close group");
                // Widen the inner expression's span to cover the
                // parentheses: the node represents this whole occurrence.
                p.nodes.items[e].start = t.start;
                p.nodes.items[e].end = p.prevEnd();
                return e;
            },
            .lbracket => return p.parseListLiteral(),
            .lbrace => return p.parseMapLiteral(),
            .dot => {
                // Absolute (leading-dot) qualified name.
                const it = p.peek(1);
                if (it.kind != .ident) return p.fail(it.start, "expected identifier after '.'");
                if (it.is_reserved) return p.fail(it.start, "reserved word cannot be used as a name");
                p.advance(); // consume '.'; parseNameOrCall starts at the ident
                return p.parseNameOrCall(true);
            },
            .ident => {
                if (t.is_reserved) return p.fail(t.start, "reserved word cannot be used as an identifier");
                return p.parseNameOrCall(false);
            },
            else => return p.fail(t.start, "unexpected token"),
        }
    }

    /// Parse an identifier chain starting at the current token, then either a
    /// call (`f(x)` / `.f(x)`), a message construction (`a.b.M{...}`), or a
    /// select chain (`a.b.c`).
    fn parseNameOrCall(p: *P, abs: bool) Error!NodeIndex {
        const first = p.cur();
        p.advance(); // consume ident

        // Global function call: f(args) or .f(args).
        if (p.cur().kind == .lparen) {
            const args = try p.parseArgList();
            if (!p.disable_macros and std.mem.eql(u8, first.text, "has") and args.len == 1) {
                return p.expandHas(args[0], first.start);
            }
            return p.call(first.text, args, first.start, false);
        }

        // Qualified name chain, possibly ending in a message literal.
        var segments = std.ArrayList([]const u8){};
        defer segments.deinit(p.alloc);
        try segments.append(p.alloc, first.text);
        var seg_ends = std.ArrayList(u32){};
        defer seg_ends.deinit(p.alloc);
        try seg_ends.append(p.alloc, first.end);
        while (p.cur().kind == .dot and p.peek(1).kind == .ident and !p.peek(1).is_reserved) {
            p.advance(); // '.'
            const seg = p.cur();
            p.advance();
            // A call terminates the name chain; rewind so the member loop
            // builds the receiver call (`a.b.c(x)` -> select a.b, call c).
            if (p.cur().kind == .lparen) {
                p.pos -= 2;
                break;
            }
            try segments.append(p.alloc, seg.text);
            try seg_ends.append(p.alloc, seg.end);
            if (p.cur().kind == .lbrace) {
                // Message construction with a qualified type name.
                return p.parseStructLiteral(segments.items, abs, first.start);
            }
        }

        // A message construction with a simple type name: `M{f: v}`.
        if (p.cur().kind == .lbrace) {
            return p.parseStructLiteral(segments.items, abs, first.start);
        }

        if (segments.items.len == 1) {
            const n = try p.addNode(.{ .kind = .ident, .start = first.start, .end = first.end, .payload = .{ .str = segments.items[0] } });
            if (abs) p.nodes.items[n].flags.abs = true;
            return n;
        }

        // Build select chain.
        const root = try p.addNode(.{ .kind = .ident, .start = first.start, .end = first.end, .payload = .{ .str = segments.items[0] } });
        if (abs) p.nodes.items[root].flags.abs = true;
        var e = root;
        for (segments.items[1..], seg_ends.items[1..]) |seg, seg_end| {
            const sel = try p.addNode(.{ .kind = .select, .start = first.start, .end = seg_end, .payload = .{ .str = seg } });
            try p.setChildren(sel, &.{e});
            e = sel;
        }
        return e;
    }

    /// Expand `has(e.f)`: mark the select node as a presence test. The
    /// select's span is widened to cover the whole `has(...)` call.
    fn expandHas(p: *P, arg: NodeIndex, start: u32) Error!NodeIndex {
        const n = p.nodes.items[arg];
        if (n.kind != .select or n.flags.test_only) {
            return p.fail(start, "invalid argument to has() macro: expected a field selection");
        }
        p.nodes.items[arg].flags.test_only = true;
        p.nodes.items[arg].start = start;
        p.nodes.items[arg].end = p.prevEnd();
        return arg;
    }

    fn parseListLiteral(p: *P) Error!NodeIndex {
        const start = p.cur().start;
        _ = try p.expect(.lbracket, "expected '['");
        var elems = std.ArrayList(NodeIndex){};
        defer elems.deinit(p.alloc);
        if (p.cur().kind != .rbracket) {
            while (true) {
                try elems.append(p.alloc, try p.parseExpr());
                if (p.cur().kind == .comma) {
                    p.advance();
                    if (p.cur().kind == .rbracket) break; // trailing comma
                    continue;
                }
                break;
            }
        }
        _ = try p.expect(.rbracket, "expected ']' to close list");
        const n = try p.addNode(.{ .kind = .list, .start = start, .end = p.prevEnd() });
        try p.setChildren(n, elems.items);
        return n;
    }

    fn parseMapLiteral(p: *P) Error!NodeIndex {
        const start = p.cur().start;
        _ = try p.expect(.lbrace, "expected '{'");
        var entries = std.ArrayList(NodeIndex){};
        defer entries.deinit(p.alloc);
        if (p.cur().kind != .rbrace) {
            while (true) {
                const k = try p.parseExpr();
                _ = try p.expect(.colon, "expected ':' in map entry");
                const v = try p.parseExpr();
                try entries.append(p.alloc, k);
                try entries.append(p.alloc, v);
                if (p.cur().kind == .comma) {
                    p.advance();
                    if (p.cur().kind == .rbrace) break; // trailing comma
                    continue;
                }
                break;
            }
        }
        _ = try p.expect(.rbrace, "expected '}' to close map");
        const n = try p.addNode(.{ .kind = .map, .start = start, .end = p.prevEnd() });
        try p.setChildren(n, entries.items);
        return n;
    }

    fn parseStructLiteral(p: *P, segments: []const []const u8, abs: bool, start: u32) Error!NodeIndex {
        // Message construction M{f: v, ...}.
        _ = try p.expect(.lbrace, "expected '{'");
        var name = std.ArrayList(u8){};
        defer name.deinit(p.alloc);
        if (abs) try name.append(p.alloc, '.');
        for (segments, 0..) |seg, i| {
            if (i > 0) try name.append(p.alloc, '.');
            try name.appendSlice(p.alloc, seg);
        }
        var entries = std.ArrayList(NodeIndex){};
        defer entries.deinit(p.alloc);
        if (p.cur().kind != .rbrace) {
            while (true) {
                const ft = p.cur();
                if (ft.kind != .ident) return p.fail(ft.start, "expected field name in message literal");
                p.advance();
                _ = try p.expect(.colon, "expected ':' after field name");
                const v = try p.parseExpr();
                const fnode = try p.addNode(.{ .kind = .string, .start = ft.start, .end = ft.end, .payload = .{ .str = ft.text } });
                try entries.append(p.alloc, fnode);
                try entries.append(p.alloc, v);
                if (p.cur().kind == .comma) {
                    p.advance();
                    if (p.cur().kind == .rbrace) break; // trailing comma
                    continue;
                }
                break;
            }
        }
        _ = try p.expect(.rbrace, "expected '}' to close message literal");
        const n = try p.addNode(.{ .kind = .struct_, .start = start, .end = p.prevEnd(), .payload = .{ .str = try name.toOwnedSlice(p.alloc) } });
        try p.setChildren(n, entries.items);
        return n;
    }
};

test {
    _ = tests;
}

const tests = struct {
    const testing = std.testing;

    /// Parse `src` into `arena` and return the AST; fails the test on error.
    fn parseOk(arena: *std.heap.ArenaAllocator, src: []const u8) !Ast {
        const r = try parseSrc(arena.allocator(), src, .{});
        return switch (r) {
            .ok => |a| a,
            .err => |e| {
                std.debug.print("parse error: {s} at {d} for `{s}`\n", .{ e.msg, e.pos, src });
                return error.TestUnexpectedResult;
            },
        };
    }

    fn expectErr(src: []const u8) !void {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const r = try parseSrc(arena.allocator(), src, .{});
        try testing.expect(r == .err);
    }

    fn kindOf(a: Ast, i: NodeIndex) ast.Kind {
        return a.nodes[i].kind;
    }

    test "literals" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const cases = .{
            .{ "1", ast.Kind.int },
            .{ "1u", ast.Kind.uint },
            .{ "1.5", ast.Kind.double },
            .{ "'s'", ast.Kind.string },
            .{ "b'x'", ast.Kind.bytes },
            .{ "true", ast.Kind.bool_ },
            .{ "null", ast.Kind.null_ },
        };
        inline for (cases) |c| {
            const a = try parseOk(&arena, c[0]);
            try testing.expectEqual(c[1], kindOf(a, a.root));
        }
    }

    test "min int literal" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const a = try parseOk(&arena, "-9223372036854775808");
        try testing.expectEqual(ast.Kind.int, kindOf(a, a.root));
        try testing.expectEqual(@as(i64, std.math.minInt(i64)), a.nodes[a.root].payload.int);
        // Plain max-int-plus-one is an error.
        try expectErr("9223372036854775808");
        // Double negation folds.
        const b = try parseOk(&arena, "--5");
        try testing.expectEqual(ast.Kind.int, kindOf(b, b.root));
        try testing.expectEqual(@as(i64, 5), b.nodes[b.root].payload.int);
        // Double negation of double also folds to the literal.
        const c = try parseOk(&arena, "--5.0");
        try testing.expectEqual(ast.Kind.double, kindOf(c, c.root));
        try testing.expectEqual(@as(f64, 5.0), c.nodes[c.root].payload.double);
    }

    test "operator precedence" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        // 1 + 2 * 3 == 7 parses as ((1 + (2*3)) == 7)
        const a = try parseOk(&arena, "1 + 2 * 3 == 7");
        try testing.expectEqual(ast.Kind.call, kindOf(a, a.root));
        try testing.expectEqualStrings("_==_", a.nodes[a.root].payload.str);
        const add = a.children(a.root)[0];
        try testing.expectEqualStrings("_+_", a.nodes[add].payload.str);
        const mul = a.children(add)[1];
        try testing.expectEqualStrings("_*_", a.nodes[mul].payload.str);
    }

    test "ternary associativity" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const a = try parseOk(&arena, "a ? b : c ? d : e");
        try testing.expectEqual(ast.Kind.ternary, kindOf(a, a.root));
        const els = a.children(a.root)[2];
        try testing.expectEqual(ast.Kind.ternary, kindOf(a, els));
    }

    test "select and index" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const a = try parseOk(&arena, "a.b[c].d");
        try testing.expectEqual(ast.Kind.select, kindOf(a, a.root));
        try testing.expectEqualStrings("d", a.nodes[a.root].payload.str);
        const idx = a.children(a.root)[0];
        try testing.expectEqual(ast.Kind.call, kindOf(a, idx));
        try testing.expectEqualStrings("_[_]", a.nodes[idx].payload.str);
    }

    test "receiver call and index call" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const a = try parseOk(&arena, "'abc'.contains('a')");
        try testing.expectEqual(ast.Kind.call, kindOf(a, a.root));
        try testing.expect(a.nodes[a.root].flags.receiver);
        const b = try parseOk(&arena, "x[1]");
        try testing.expectEqualStrings("_[_]", b.nodes[b.root].payload.str);
    }

    test "macros" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const cases = .{
            .{ "[1, 2].all(x, x > 0)", MacroKind.all },
            .{ "[1].exists(x, x > 0)", MacroKind.exists },
            .{ "[1].exists_one(x, x > 0)", MacroKind.exists_one },
            .{ "[1].filter(x, x > 0)", MacroKind.filter },
            .{ "[1].map(x, x * 2)", MacroKind.map },
            .{ "[1].map(x, x > 0, x * 2)", MacroKind.map_filter },
        };
        inline for (cases) |c| {
            const a = try parseOk(&arena, c[0]);
            try testing.expectEqual(ast.Kind.macro, kindOf(a, a.root));
            try testing.expectEqual(c[1], a.nodes[a.root].payload.macro);
            // children: [var ident, range, predicate...]
            const kids = a.children(a.root);
            try testing.expectEqual(ast.Kind.ident, kindOf(a, kids[0]));
            try testing.expectEqualStrings("x", a.nodes[kids[0]].payload.str);
        }
        // has() marks the select.
        const h = try parseOk(&arena, "has(a.b)");
        try testing.expectEqual(ast.Kind.select, kindOf(h, h.root));
        try testing.expect(h.nodes[h.root].flags.test_only);
        // Wrong-shape macro stays a call.
        const w = try parseOk(&arena, "[1].all(x)");
        try testing.expectEqual(ast.Kind.call, kindOf(w, w.root));
        try testing.expectEqualStrings("all", w.nodes[w.root].payload.str);
        // disable_macros keeps has() as a call.
        const d = try parseSrc(arena.allocator(), "has(a.b)", .{ .disable_macros = true });
        try testing.expect(d == .ok);
        try testing.expectEqual(ast.Kind.call, kindOf(d.ok, d.ok.root));
    }

    test "reserved words" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        try expectErr("as");
        // Reserved words are permitted as selectors and struct field names
        // (parse.textproto); they are rejected as plain identifiers and
        // struct type names.
        _ = try parseOk(&arena, "a.while");
        _ = try parseOk(&arena, "M{package: 1}");
        try expectErr("while{f: 1}");
        // Receiver-call style with reserved word is allowed.
        const a = try parseOk(&arena, "a.package(1)");
        try testing.expectEqual(ast.Kind.call, kindOf(a, a.root));
        try testing.expectEqualStrings("package", a.nodes[a.root].payload.str);
    }

    test "trailing commas" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        _ = try parseOk(&arena, "[1, 2,]");
        _ = try parseOk(&arena, "{1: 2,}");
    }

    test "parse errors" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        try expectErr("");
        try expectErr("1 +");
        try expectErr("(1");
        try expectErr("1 2");
        try expectErr("[1, 2");
        try expectErr("{1: ");
        try expectErr("a.");
        try expectErr("has(a)");
        try expectErr("has(a.b.c())");
        try expectErr("?");
        try expectErr(")");
    }

    test "absolute name" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const a = try parseOk(&arena, ".a.b");
        // Root select over absolute ident a.
        try testing.expectEqual(ast.Kind.select, kindOf(a, a.root));
        const root_ident = a.children(a.root)[0];
        try testing.expectEqual(ast.Kind.ident, kindOf(a, root_ident));
        try testing.expect(a.nodes[root_ident].flags.abs);
        const b = try parseOk(&arena, ".a");
        try testing.expect(b.nodes[b.root].flags.abs);
    }

    test "message literal parses" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const a = try parseOk(&arena, "google.protobuf.Int32Value{value: 1}");
        try testing.expectEqual(ast.Kind.struct_, kindOf(a, a.root));
        try testing.expectEqualStrings("google.protobuf.Int32Value", a.nodes[a.root].payload.str);
    }

    test "qualified chain then call" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        // a.b.c(1) -> receiver call c on select a.b.
        const a = try parseOk(&arena, "a.b.c(1)");
        try testing.expectEqual(ast.Kind.call, kindOf(a, a.root));
        try testing.expectEqualStrings("c", a.nodes[a.root].payload.str);
        try testing.expect(a.nodes[a.root].flags.receiver);
        const target = a.children(a.root)[0];
        try testing.expectEqual(ast.Kind.select, kindOf(a, target));
        try testing.expectEqualStrings("b", a.nodes[target].payload.str);
    }

    test "node spans" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();

        // Operator call nodes span the whole expression, not just the
        // operator token.
        {
            const a = try parseOk(&arena, "1 + 2");
            try testing.expectEqual(@as(u32, 0), a.nodes[a.root].start);
            try testing.expectEqual(@as(u32, 5), a.nodes[a.root].end);
            const lhs = a.children(a.root)[0];
            const rhs = a.children(a.root)[1];
            try testing.expectEqual(@as(u32, 0), a.nodes[lhs].start);
            try testing.expectEqual(@as(u32, 1), a.nodes[lhs].end);
            try testing.expectEqual(@as(u32, 4), a.nodes[rhs].start);
            try testing.expectEqual(@as(u32, 5), a.nodes[rhs].end);
        }
        // Function call and its arguments.
        {
            const a = try parseOk(&arena, "size(items) > 3");
            try testing.expectEqual(@as(u32, 0), a.nodes[a.root].start);
            try testing.expectEqual(@as(u32, 15), a.nodes[a.root].end);
            const call = a.children(a.root)[0];
            try testing.expectEqual(@as(u32, 0), a.nodes[call].start);
            try testing.expectEqual(@as(u32, 11), a.nodes[call].end);
            const arg = a.children(call)[0];
            try testing.expectEqual(@as(u32, 5), a.nodes[arg].start);
            try testing.expectEqual(@as(u32, 10), a.nodes[arg].end);
        }
        // Parentheses widen the inner node's span; the outer operator
        // starts at the opening parenthesis.
        {
            const a = try parseOk(&arena, "(1 + 2) * 3");
            try testing.expectEqual(@as(u32, 0), a.nodes[a.root].start);
            try testing.expectEqual(@as(u32, 11), a.nodes[a.root].end);
            const lhs = a.children(a.root)[0];
            try testing.expectEqual(@as(u32, 0), a.nodes[lhs].start);
            try testing.expectEqual(@as(u32, 7), a.nodes[lhs].end);
        }
        // Ternary, macro, list, map.
        {
            const t = try parseOk(&arena, "a ? b : c");
            try testing.expectEqual(@as(u32, 0), t.nodes[t.root].start);
            try testing.expectEqual(@as(u32, 9), t.nodes[t.root].end);
        }
        {
            const m = try parseOk(&arena, "[1, 2, 3].map(x, x * 2)");
            try testing.expectEqual(ast.Kind.macro, kindOf(m, m.root));
            try testing.expectEqual(@as(u32, 0), m.nodes[m.root].start);
            try testing.expectEqual(@as(u32, 23), m.nodes[m.root].end);
            const kids = m.children(m.root);
            try testing.expectEqual(@as(u32, 14), m.nodes[kids[0]].start);
            try testing.expectEqual(@as(u32, 15), m.nodes[kids[0]].end);
            try testing.expectEqual(@as(u32, 0), m.nodes[kids[1]].start);
            try testing.expectEqual(@as(u32, 9), m.nodes[kids[1]].end);
            try testing.expectEqual(@as(u32, 17), m.nodes[kids[2]].start);
            try testing.expectEqual(@as(u32, 22), m.nodes[kids[2]].end);
        }
        {
            const mp = try parseOk(&arena, "{1: 'a'}");
            try testing.expectEqual(@as(u32, 0), mp.nodes[mp.root].start);
            try testing.expectEqual(@as(u32, 8), mp.nodes[mp.root].end);
        }
        // Receiver calls and select chains root at the receiver.
        {
            const r = try parseOk(&arena, "'abc'.contains('a')");
            try testing.expectEqual(@as(u32, 0), r.nodes[r.root].start);
            try testing.expectEqual(@as(u32, 19), r.nodes[r.root].end);
            const recv = r.children(r.root)[0];
            try testing.expectEqual(@as(u32, 0), r.nodes[recv].start);
            try testing.expectEqual(@as(u32, 5), r.nodes[recv].end);
        }
        {
            const s = try parseOk(&arena, "a.b[c].d");
            try testing.expectEqual(@as(u32, 0), s.nodes[s.root].start);
            try testing.expectEqual(@as(u32, 8), s.nodes[s.root].end);
            const idx = s.children(s.root)[0];
            try testing.expectEqual(@as(u32, 0), s.nodes[idx].start);
            try testing.expectEqual(@as(u32, 6), s.nodes[idx].end);
            const sel = s.children(idx)[0];
            try testing.expectEqual(@as(u32, 0), s.nodes[sel].start);
            try testing.expectEqual(@as(u32, 3), s.nodes[sel].end);
        }
        // has() widens the select to the whole macro call.
        {
            const h = try parseOk(&arena, "has(a.b)");
            try testing.expect(h.nodes[h.root].flags.test_only);
            try testing.expectEqual(@as(u32, 0), h.nodes[h.root].start);
            try testing.expectEqual(@as(u32, 8), h.nodes[h.root].end);
        }
        // Unary folds keep the full extent.
        {
            const n = try parseOk(&arena, "--5");
            try testing.expectEqual(ast.Kind.int, kindOf(n, n.root));
            try testing.expectEqual(@as(u32, 0), n.nodes[n.root].start);
            try testing.expectEqual(@as(u32, 3), n.nodes[n.root].end);
            const b = try parseOk(&arena, "!!x");
            try testing.expectEqual(ast.Kind.ident, kindOf(b, b.root));
            try testing.expectEqual(@as(u32, 0), b.nodes[b.root].start);
            try testing.expectEqual(@as(u32, 3), b.nodes[b.root].end);
            const g = try parseOk(&arena, "-x");
            try testing.expectEqual(ast.Kind.neg, kindOf(g, g.root));
            try testing.expectEqual(@as(u32, 0), g.nodes[g.root].start);
            try testing.expectEqual(@as(u32, 2), g.nodes[g.root].end);
        }
    }

    test "deep nesting bounded" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var buf = std.ArrayList(u8){};
        defer buf.deinit(testing.allocator);
        for (0..300) |_| try buf.append(testing.allocator, '(');
        try buf.appendSlice(testing.allocator, "1");
        for (0..300) |_| try buf.append(testing.allocator, ')');
        try expectErr(buf.items);
    }
};
