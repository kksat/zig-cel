//! Tree-walking CEL evaluator with the standard function library.
//!
//! Semantics follow cel-spec/doc/langdef.md:
//! - `&&`/`||` are commutative with error absorption (`error && false` is
//!   `false`), `?:` evaluates only the taken branch;
//! - equality is heterogeneous (numeric values compare on a shared number
//!   line; NaN != NaN); ordering comparisons involving NaN are `false`;
//! - operators dispatch as functions (`_+_`, `@in`, `_[_]`, ...);
//! - macros (all/exists/exists_one/filter/map) iterate lists and map keys
//!   with the logical-operator error rules for all/exists;
//! - `has(e.f)` tests map key presence;
//! - durations and timestamps support arithmetic, conversions, and the
//!   get*() accessors (UTC, fixed offsets, and the bundled named zones).

const std = @import("std");
const ast = @import("ast.zig");
const value_mod = @import("value.zig");
const stdlib = @import("stdlib.zig");

pub const Value = value_mod.Value;
pub const Map = value_mod.Map;

pub const Error = error{ Eval, OutOfMemory };

pub const Binding = stdlib.Binding;

const Scope = struct {
    name: []const u8,
    value: Value,
};

pub const Evaluator = struct {
    alloc: std.mem.Allocator,
    tree: ast.Ast,
    vars: []const Binding = &.{},
    /// Optional protobuf package container for name resolution (conformance
    /// `container`); qualified names are resolved against its prefixes.
    container: []const u8 = "",
    scopes: std.ArrayList(Scope) = .empty,
    ctx: stdlib.Ctx,

    const e = Evaluator;

    pub fn init(alloc: std.mem.Allocator, tree: ast.Ast) Evaluator {
        return .{ .alloc = alloc, .tree = tree, .ctx = stdlib.Ctx.init(alloc) };
    }

    pub fn run(ev: *Evaluator, vars: []const Binding) Error!Value {
        ev.vars = vars;
        ev.scopes.clearRetainingCapacity();
        return ev.eval(ev.tree.root);
    }

    fn fail(ev: *Evaluator, comptime fmt: []const u8, args: anytype) Error {
        return ev.ctx.fail(fmt, args);
    }

    fn failUnbound(ev: *Evaluator, name: []const u8) Error {
        return ev.ctx.failUnbound(name);
    }

    /// Evaluate, returning null instead of propagating Eval errors (used by
    /// error-absorbing operators and macros).
    fn tryEval(ev: *Evaluator, node: ast.NodeIndex) Error!?Value {
        return ev.eval(node) catch |err| switch (err) {
            error.Eval => null,
            error.OutOfMemory => error.OutOfMemory,
        };
    }

    fn child(ev: *Evaluator, node: ast.NodeIndex) ast.NodeIndex {
        const n = ev.tree.nodes[node];
        return ev.tree.child_table[n.child];
    }

    fn children(ev: *Evaluator, node: ast.NodeIndex) []const ast.NodeIndex {
        return ev.tree.children(node);
    }

    fn eval(ev: *Evaluator, node: ast.NodeIndex) Error!Value {
        const n = ev.tree.nodes[node];
        switch (n.kind) {
            .int => return .{ .int_t = n.payload.int },
            .uint => return .{ .uint_t = n.payload.uint },
            .double => return .{ .double_t = n.payload.double },
            .string => return .{ .string_t = n.payload.str },
            .bytes => return .{ .bytes_t = n.payload.str },
            .bool_ => return .{ .bool_t = n.payload.int != 0 },
            .null_ => return .{ .null_t = {} },

            .ident => return ev.evalIdent(node),
            .select => return ev.evalSelect(node),
            .call => return ev.evalCall(node),
            .list => {
                const kids = ev.children(node);
                const items = try ev.alloc.alloc(Value, kids.len);
                for (kids, 0..) |k, i| items[i] = try ev.eval(k);
                return .{ .list_t = items };
            },
            .map => return ev.evalMapLiteral(node),
            .struct_ => return ev.fail("unsupported: message literal '{s}' (protobuf messages are not implemented)", .{n.payload.str}),

            .not => {
                const v = try ev.eval(ev.child(node));
                switch (v) {
                    .bool_t => |b| return .{ .bool_t = !b },
                    else => return ev.ctx.noOverload(.not, &.{v}),
                }
            },
            .neg => {
                const v = try ev.eval(ev.child(node));
                switch (v) {
                    .int_t => |i| {
                        if (i == std.math.minInt(i64)) return ev.fail("return error for overflow: negation of min int", .{});
                        return .{ .int_t = -i };
                    },
                    .double_t => |d| return .{ .double_t = -d },
                    else => return ev.ctx.noOverload(.neg, &.{v}),
                }
            },
            .and_ => {
                const kids = ev.children(node);
                return ev.evalLogical(kids[0], kids[1], .and_op);
            },
            .or_ => {
                const kids = ev.children(node);
                return ev.evalLogical(kids[0], kids[1], .or_op);
            },
            .ternary => {
                const kids = ev.children(node);
                const cond = try ev.eval(kids[0]);
                switch (cond) {
                    .bool_t => |b| return ev.eval(if (b) kids[1] else kids[2]),
                    else => return ev.ctx.noOverload(.ternary, &.{cond}),
                }
            },
            .macro => return ev.evalMacro(node),
        }
    }

    // ---- identifiers and selection ----

    /// Look up a name in comprehension scopes only.
    fn lookupScope(ev: *Evaluator, name: []const u8) ?Value {
        var i = ev.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, ev.scopes.items[i].name, name)) return ev.scopes.items[i].value;
        }
        return null;
    }

    fn lookupVar(ev: *Evaluator, name: []const u8) ?Value {
        if (ev.lookupScope(name)) |v| return v;
        return ev.lookupTopLevel(name);
    }

    /// Look up a name in the top-level bindings only (no comprehension
    /// scopes, no container).
    fn lookupTopLevel(ev: *Evaluator, name: []const u8) ?Value {
        for (ev.vars) |v| {
            if (std.mem.eql(u8, v.name, name)) return v.value;
        }
        return null;
    }

    /// Iterate the container prefixes for name resolution, longest first,
    /// ending with the empty prefix (bare name).
    fn containerPrefixes(container: []const u8, buf: *std.ArrayList([]const u8), alloc: std.mem.Allocator) Error!void {
        if (container.len > 0) {
            var end = container.len;
            while (true) {
                try buf.append(alloc, container[0..end]);
                const dot = std.mem.lastIndexOfScalar(u8, container[0..end], '.') orelse break;
                end = dot;
            }
        }
        try buf.append(alloc, "");
    }

    fn evalIdent(ev: *Evaluator, node: ast.NodeIndex) Error!Value {
        const n = ev.tree.nodes[node];
        if (n.flags.abs) {
            // Absolute names (`.y`) resolve only in the root scope,
            // bypassing comprehension scopes and the container.
            if (ev.lookupTopLevel(n.payload.str)) |v| return v;
        } else {
            // Comprehension scopes shadow outer names.
            if (ev.lookupScope(n.payload.str)) |v| return v;
            // Container-qualified resolution takes precedence over the bare
            // name (namespace.textproto: self_eval_container_lookup).
            if (ev.container.len > 0) {
                var prefixes = std.ArrayList([]const u8){};
                defer prefixes.deinit(ev.alloc);
                try containerPrefixes(ev.container, &prefixes, ev.alloc);
                for (prefixes.items) |pfx| {
                    if (pfx.len == 0) break;
                    const q = std.fmt.allocPrint(ev.alloc, "{s}.{s}", .{ pfx, n.payload.str }) catch return error.OutOfMemory;
                    if (ev.lookupTopLevel(q)) |v| return v;
                }
            }
            if (ev.lookupTopLevel(n.payload.str)) |v| return v;
        }
        if (stdlib.bareTypeDenotation(n.payload.str)) |t| return .{ .type_t = t };
        return ev.failUnbound(n.payload.str);
    }

    const QualifiedName = struct { name: []const u8, abs: bool };

    /// Collect the dotted name of a pure select chain rooted at an ident,
    /// e.g. `google.protobuf.Timestamp`. Returns null for anything else.
    fn qualifiedName(ev: *Evaluator, node: ast.NodeIndex) ?QualifiedName {
        var parts = std.ArrayList([]const u8){};
        defer parts.deinit(ev.alloc);
        var abs = false;
        var cur = node;
        while (true) {
            const n = ev.tree.nodes[cur];
            switch (n.kind) {
                .ident => {
                    abs = n.flags.abs;
                    parts.append(ev.alloc, n.payload.str) catch return null;
                    break;
                },
                .select => {
                    if (n.flags.test_only) return null;
                    parts.append(ev.alloc, n.payload.str) catch return null;
                    cur = ev.child(cur);
                },
                else => return null,
            }
        }
        std.mem.reverse([]const u8, parts.items);
        var out = std.ArrayList(u8){};
        defer out.deinit(ev.alloc);
        for (parts.items, 0..) |p, i| {
            if (i > 0) out.append(ev.alloc, '.') catch return null;
            out.appendSlice(ev.alloc, p) catch return null;
        }
        const name = out.toOwnedSlice(ev.alloc) catch return null;
        return .{ .name = name, .abs = abs };
    }

    fn evalSelect(ev: *Evaluator, node: ast.NodeIndex) Error!Value {
        const n = ev.tree.nodes[node];
        const operand_node = ev.child(node);
        const operand = ev.eval(operand_node) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Eval => {
                // A select chain rooted at an unbound name may resolve as a
                // qualified variable binding or a well-known type denotation
                // (google.protobuf.Timestamp), possibly container-qualified.
                if (ev.ctx.unbound) {
                    if (ev.qualifiedName(node)) |q| {
                        // Absolute names (`.a.b`) resolve only in the root
                        // scope; others try the container prefixes first.
                        var prefixes = std.ArrayList([]const u8){};
                        defer prefixes.deinit(ev.alloc);
                        if (q.abs) {
                            try prefixes.append(ev.alloc, "");
                        } else {
                            try containerPrefixes(ev.container, &prefixes, ev.alloc);
                        }
                        for (prefixes.items) |pfx| {
                            const candidate = if (pfx.len == 0) q.name else std.fmt.allocPrint(ev.alloc, "{s}.{s}", .{ pfx, q.name }) catch return error.OutOfMemory;
                            if (ev.lookupTopLevel(candidate)) |v| return v;
                        }
                        for (prefixes.items) |pfx| {
                            const candidate = if (pfx.len == 0) q.name else std.fmt.allocPrint(ev.alloc, "{s}.{s}", .{ pfx, q.name }) catch return error.OutOfMemory;
                            if (value_mod.Type.fromName(candidate)) |t| {
                                return .{ .type_t = t };
                            }
                        }
                    }
                }
                return error.Eval;
            },
        };
        const field = n.payload.str;
        switch (operand) {
            .map_t => |m| {
                const key = Value{ .string_t = field };
                if (n.flags.test_only) {
                    return .{ .bool_t = m.has(key) };
                }
                return m.get(key) orelse ev.fail("no such key: '{s}'", .{field});
            },
            else => {
                if (n.flags.test_only) {
                    return ev.fail("no such field: has() is only supported on maps", .{});
                }
                return ev.fail("no such field: cannot select field '{s}' of {s}", .{ field, operand.typeOf().name() });
            },
        }
    }

    fn evalMapLiteral(ev: *Evaluator, node: ast.NodeIndex) Error!Value {
        const kids = ev.children(node);
        std.debug.assert(kids.len % 2 == 0);
        const count = kids.len / 2;
        const entries = try ev.alloc.alloc(Map.Entry, count);
        for (0..count) |i| {
            const k = try ev.eval(kids[i * 2]);
            switch (k) {
                .int_t, .uint_t, .bool_t, .string_t => {},
                else => return ev.fail("bad key type: map key must be int, uint, bool, or string, not {s}", .{k.typeOf().name()}),
            }
            entries[i] = .{ .key = k, .value = try ev.eval(kids[i * 2 + 1]) };
        }
        const m = Map.init(ev.alloc, entries) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.RepeatedKey => return ev.fail("Failed with repeated key", .{}),
        };
        return .{ .map_t = m };
    }

    // ---- logical operators ----

    const LogicalOp = enum { and_op, or_op };

    fn evalLogical(ev: *Evaluator, lhs_node: ast.NodeIndex, rhs_node: ast.NodeIndex, op: LogicalOp) Error!Value {
        const absorbing: bool = switch (op) {
            .and_op => false, // `false && x` = false
            .or_op => true, // `true || x` = true
        };
        const fn_id: stdlib.FnId = if (op == .and_op) .and_msg else .or_msg;
        const lhs = try ev.tryEval(lhs_node);
        if (lhs) |v| {
            if (v == .bool_t and v.bool_t == absorbing) return .{ .bool_t = absorbing };
        }
        const rhs = try ev.tryEval(rhs_node);
        if (rhs) |v| {
            if (v == .bool_t and v.bool_t == absorbing) return .{ .bool_t = absorbing };
        }
        // Neither operand determined the result: any error or non-bool
        // operand is now a hard failure.
        if (lhs) |v| {
            if (v != .bool_t) return ev.ctx.noOverload(fn_id, &.{v});
        }
        if (rhs) |v| {
            if (v != .bool_t) return ev.ctx.noOverload(fn_id, &.{v});
        }
        if (lhs != null and rhs != null) {
            // Both bools, neither absorbing: true && true, false || false.
            return .{ .bool_t = !absorbing };
        }
        return ev.fail("{s}", .{if (ev.ctx.err_msg.len > 0) ev.ctx.err_msg else "evaluation error"});
    }

    // ---- calls ----

    fn evalCall(ev: *Evaluator, node: ast.NodeIndex) Error!Value {
        const n = ev.tree.nodes[node];
        const kids = ev.children(node);
        const name = n.payload.str;

        // Evaluate all arguments (strict evaluation).
        var args = std.ArrayList(Value){};
        defer args.deinit(ev.alloc);
        for (kids) |k| {
            try args.append(ev.alloc, try ev.eval(k));
        }
        return stdlib.callByName(&ev.ctx, name, args.items);
    }

    // ---- macros (comprehensions) ----

    const Range = struct {
        items: []const Value,

        fn init(ev: *Evaluator, v: Value) Error!Range {
            switch (v) {
                .list_t => |l| return .{ .items = l },
                .map_t => |m| {
                    const keys = try ev.alloc.alloc(Value, m.entries.len);
                    for (m.entries, 0..) |entry, i| keys[i] = entry.key;
                    return .{ .items = keys };
                },
                else => return ev.fail("no_matching_overload: cannot range over {s}", .{v.typeOf().name()}),
            }
        }
    };

    fn evalMacro(ev: *Evaluator, node: ast.NodeIndex) Error!Value {
        const n = ev.tree.nodes[node];
        const kids = ev.children(node);
        // kids: [iter_var, range, predicate, transform?]
        const var_name = ev.tree.nodes[kids[0]].payload.str;
        const range_val = try ev.eval(kids[1]);
        const range = try Range.init(ev, range_val);
        const pred = kids[2];
        const has_transform = kids.len > 3;

        switch (n.payload.macro) {
            .all, .exists => {
                const is_all = n.payload.macro == .all;
                var saw_error = false;
                for (range.items) |item| {
                    try ev.scopes.append(ev.alloc, .{ .name = var_name, .value = item });
                    defer _ = ev.scopes.pop();
                    const pv = try ev.tryEval(pred);
                    if (pv) |p| {
                        switch (p) {
                            .bool_t => |b| {
                                // all: false short-circuits; exists: true does.
                                if (b == !is_all) return .{ .bool_t = !is_all };
                            },
                            else => {
                                return ev.ctx.noOverload(if (is_all) .macro_all else .macro_exists, &.{p});
                            },
                        }
                    } else {
                        saw_error = true;
                    }
                }
                if (saw_error) {
                    return ev.fail("{s}", .{if (ev.ctx.err_msg.len > 0) ev.ctx.err_msg else "evaluation error"});
                }
                return .{ .bool_t = is_all };
            },
            .exists_one => {
                var count: usize = 0;
                for (range.items) |item| {
                    try ev.scopes.append(ev.alloc, .{ .name = var_name, .value = item });
                    defer _ = ev.scopes.pop();
                    const p = try ev.eval(pred);
                    switch (p) {
                        .bool_t => |b| {
                            if (b) count += 1;
                        },
                        else => return ev.ctx.noOverload(.macro_exists_one, &.{p}),
                    }
                }
                return .{ .bool_t = count == 1 };
            },
            .filter => {
                var out = std.ArrayList(Value){};
                defer out.deinit(ev.alloc);
                for (range.items) |item| {
                    try ev.scopes.append(ev.alloc, .{ .name = var_name, .value = item });
                    defer _ = ev.scopes.pop();
                    const p = try ev.eval(pred);
                    switch (p) {
                        .bool_t => |b| {
                            if (b) try out.append(ev.alloc, item);
                        },
                        else => return ev.ctx.noOverload(.macro_filter, &.{p}),
                    }
                }
                return .{ .list_t = try ev.alloc.dupe(Value, out.items) };
            },
            .map, .map_filter => {
                var out = std.ArrayList(Value){};
                defer out.deinit(ev.alloc);
                const transform = if (has_transform) kids[3] else pred;
                const filter = if (has_transform) pred else null;
                for (range.items) |item| {
                    try ev.scopes.append(ev.alloc, .{ .name = var_name, .value = item });
                    defer _ = ev.scopes.pop();
                    if (filter) |f| {
                        const p = try ev.eval(f);
                        switch (p) {
                            .bool_t => |b| {
                                if (!b) continue;
                            },
                            else => return ev.ctx.noOverload(.macro_map, &.{p}),
                        }
                    }
                    try out.append(ev.alloc, try ev.eval(transform));
                }
                return .{ .list_t = try ev.alloc.dupe(Value, out.items) };
            },
        }
    }
};

test {
    _ = tests;
}

const tests = struct {
    const testing = std.testing;
    const parser = @import("parser.zig");

    const Case = struct {
        expr: []const u8,
        want: ?[]const u8 = null, // display form; null means eval error
        vars: []const Binding = &.{},
    };

    /// Evaluate `expr` and return its display form (allocated with the test
    /// allocator), or null on evaluation error. The parse/eval arena is
    /// released before returning; only the copied display string survives.
    fn evalToDisplay(src: []const u8, vars: []const Binding) !?[]const u8 {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const r = try parser.parseSrc(arena.allocator(), src, .{});
        const tree = switch (r) {
            .ok => |t| t,
            .err => {
                std.debug.print("parse error (expr: {s})\n", .{src});
                return null;
            },
        };
        var ev = Evaluator.init(arena.allocator(), tree);
        const v = ev.run(vars) catch |err| switch (err) {
            error.Eval => {
                std.debug.print("eval error: {s} (expr: {s})\n", .{ ev.ctx.err_msg, src });
                return null;
            },
            else => return error.OutOfMemory,
        };
        var aw = std.Io.Writer.Allocating.init(arena.allocator());
        try v.format(&aw.writer);
        return try testing.allocator.dupe(u8, aw.written());
    }

    fn check(cases: []const Case) !void {
        for (cases) |c| {
            const got = try evalToDisplay(c.expr, c.vars);
            if (c.want) |want| {
                if (got == null) {
                    std.debug.print("expected `{s}` but got error for expr: {s}\n", .{ want, c.expr });
                    return error.TestUnexpectedResult;
                }
                testing.expectEqualStrings(want, got.?) catch |e| {
                    std.debug.print("expr: {s}\n", .{c.expr});
                    testing.allocator.free(got.?);
                    return e;
                };
                testing.allocator.free(got.?);
            } else {
                if (got != null) {
                    std.debug.print("expected error but got `{s}` for expr: {s}\n", .{ got.?, c.expr });
                    testing.allocator.free(got.?);
                    return error.TestUnexpectedResult;
                }
            }
        }
    }

    test "arithmetic and literals" {
        try check(&.{
            .{ .expr = "1 + 2", .want = "3" },
            .{ .expr = "1 + 2 * 3", .want = "7" },
            .{ .expr = "7 / 2", .want = "3" },
            .{ .expr = "7 % 2", .want = "1" },
            .{ .expr = "7.0 / 2.0", .want = "3.5" },
            .{ .expr = "2u + 3u", .want = "5" },
            .{ .expr = "-5 + 3", .want = "-2" },
            .{ .expr = "-9223372036854775808", .want = "-9223372036854775808" },
            .{ .expr = "1 + 9223372036854775807", .want = null },
            .{ .expr = "1 / 0", .want = null },
            .{ .expr = "1 % 0", .want = null },
            .{ .expr = "1 + 1u", .want = null },
            .{ .expr = "1 % 1.0", .want = null },
            .{ .expr = "'a' + 'b'", .want = "ab" },
            .{ .expr = "b'a' + b'b'", .want = "b\"ab\"" },
            .{ .expr = "[1] + [2, 3]", .want = "[1, 2, 3]" },
            .{ .expr = "-1u", .want = null },
        });
    }

    test "comparisons and equality" {
        try check(&.{
            .{ .expr = "1 < 2", .want = "true" },
            .{ .expr = "1.0 == 1", .want = "true" },
            .{ .expr = "1u == 1.0", .want = "true" },
            .{ .expr = "1 != 1.0", .want = "false" },
            .{ .expr = "'a' < 'b'", .want = "true" },
            .{ .expr = "true > false", .want = "true" },
            .{ .expr = "1 < 'a'", .want = null },
            .{ .expr = "0.0 == -0.0", .want = "true" },
            .{ .expr = "[1, 2] == [1, 2]", .want = "true" },
            .{ .expr = "[1, 2] == [1, 2.0]", .want = "true" },
            .{ .expr = "{1: 'a'} == {1u: 'a'}", .want = "true" },
            .{ .expr = "1 in [1, 2]", .want = "true" },
            .{ .expr = "1.0 in [1, 2]", .want = "true" },
            .{ .expr = "3 in {1: 'a', 3: 'b'}", .want = "true" },
            .{ .expr = "5 in [1, 2]", .want = "false" },
        });
    }

    test "logic" {
        try check(&.{
            .{ .expr = "true && false", .want = "false" },
            .{ .expr = "true || false", .want = "true" },
            .{ .expr = "!true", .want = "false" },
            .{ .expr = "(1/0 == 1) || true", .want = "true" },
            .{ .expr = "(1/0 == 1) || false", .want = null },
            .{ .expr = "(1/0 == 1) && false", .want = "false" },
            .{ .expr = "(1/0 == 1) && true", .want = null },
            .{ .expr = "true ? 1 : 2", .want = "1" },
            .{ .expr = "false ? 1 : 2", .want = "2" },
            .{ .expr = "false ? 1 : 1/0 == 1", .want = null },
            .{ .expr = "1 && true", .want = null },
        });
    }

    test "variables and selection" {
        const list = [_]Value{ .{ .int_t = 1 }, .{ .int_t = 2 } };
        var entries = [_]Map.Entry{
            .{ .key = .{ .string_t = "a" }, .value = .{ .int_t = 10 } },
        };
        var map_arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer map_arena.deinit();
        const m = try Map.init(map_arena.allocator(), &entries);
        var vars = [_]Binding{
            .{ .name = "x", .value = .{ .int_t = 42 } },
            .{ .name = "items", .value = .{ .list_t = &list } },
            .{ .name = "m", .value = .{ .map_t = m } },
        };
        try check(&.{
            .{ .expr = "x", .want = "42", .vars = &vars },
            .{ .expr = "items[1]", .want = "2", .vars = &vars },
            .{ .expr = "items[2]", .want = null, .vars = &vars },
            .{ .expr = "items[1.5]", .want = null, .vars = &vars },
            .{ .expr = "items[1.0]", .want = "2", .vars = &vars },
            .{ .expr = "items[0u]", .want = "1", .vars = &vars },
            .{ .expr = "m.a", .want = "10", .vars = &vars },
            .{ .expr = "m['a']", .want = "10", .vars = &vars },
            .{ .expr = "has(m.a)", .want = "true", .vars = &vars },
            .{ .expr = "has(m.b)", .want = "false", .vars = &vars },
            .{ .expr = "m.b", .want = null, .vars = &vars },
            .{ .expr = "y", .want = null, .vars = &vars },
            .{ .expr = "size(items)", .want = "2", .vars = &vars },
            .{ .expr = "size(m)", .want = "1", .vars = &vars },
        });
    }

    test "functions" {
        try check(&.{
            .{ .expr = "size('h\u{00e9}llo')", .want = "5" },
            .{ .expr = "size(b'abc')", .want = "3" },
            .{ .expr = "'hello world'.contains('world')", .want = "true" },
            .{ .expr = "'hello'.startsWith('he')", .want = "true" },
            .{ .expr = "'hello'.endsWith('lo')", .want = "true" },
            .{ .expr = "'banana'.matches('ba(na)*')", .want = "true" },
            .{ .expr = "'banana'.matches('^cherry$')", .want = "false" },
            .{ .expr = "int('123')", .want = "123" },
            .{ .expr = "int('-123')", .want = "-123" },
            .{ .expr = "int('abc')", .want = null },
            .{ .expr = "int(3.9)", .want = "3" },
            .{ .expr = "int(-3.9)", .want = "-3" },
            .{ .expr = "int(1e100)", .want = null },
            .{ .expr = "uint('42')", .want = "42" },
            .{ .expr = "uint(-1)", .want = null },
            .{ .expr = "double('3.14')", .want = "3.14" },
            .{ .expr = "string(123)", .want = "123" },
            .{ .expr = "string(123.456)", .want = "123.456" },
            .{ .expr = "string(-4.5e-3)", .want = "-0.0045" },
            .{ .expr = "string(true)", .want = "true" },
            .{ .expr = "string(b'abc')", .want = "abc" },
            .{ .expr = "bool('TRUE')", .want = "true" },
            .{ .expr = "bool('TrUe')", .want = null },
            .{ .expr = "type(1)", .want = "int" },
            .{ .expr = "type('a')", .want = "string" },
            .{ .expr = "type([1]) == list", .want = "true" },
            .{ .expr = "dyn(5) + 1", .want = "6" },
        });
    }

    test "macros" {
        try check(&.{
            .{ .expr = "[1, 2, 3].all(x, x > 0)", .want = "true" },
            .{ .expr = "[1, 0, 3].all(x, x > 0)", .want = "false" },
            .{ .expr = "[0, 2, 3].all(x, x > 0)", .want = "false" },
            .{ .expr = "[1, 2, 3].exists(x, x == 2)", .want = "true" },
            .{ .expr = "[1, 2, 3].exists_one(x, x > 2)", .want = "true" },
            .{ .expr = "[1, 2, 3].exists_one(x, x > 1)", .want = "false" },
            .{ .expr = "[1, 2, 3].filter(x, x % 2 == 1)", .want = "[1, 3]" },
            .{ .expr = "[1, 2, 3].map(x, x * 2)", .want = "[2, 4, 6]" },
            .{ .expr = "[1, 2, 3, 4].map(x, x % 2 == 0, x * 2)", .want = "[4, 8]" },
            .{ .expr = "{'a': 1, 'b': 2}.map(k, k)", .want = "[a, b]" },
            .{ .expr = "{'a': 1}.all(k, k == 'a')", .want = "true" },
            .{ .expr = "[1, 2].all(x, x in [1, 2])", .want = "true" },
            .{ .expr = "[1].all(x, x > 0)", .want = "true" },
            .{ .expr = "[1, 2].all(x, [1].exists(y, x == y))", .want = "false" },
            .{ .expr = "[2, 1, 0].map(n, 4 / n)", .want = null },
            .{ .expr = "[1, 2].exists(x, x > 1 && x / 0 == 0)", .want = null },
            .{ .expr = "[2, 1].exists(x, x > 1 || x / 0 == 0)", .want = "true" },
        });
    }

    test "timestamps and durations" {
        try check(&.{
            .{ .expr = "timestamp('2009-02-13T23:31:30Z')", .want = "timestamp(2009-02-13T23:31:30Z)" },
            .{ .expr = "string(timestamp('2009-02-13T23:31:30Z'))", .want = "2009-02-13T23:31:30Z" },
            .{ .expr = "int(timestamp('2009-02-13T23:31:30Z'))", .want = "1234567890" },
            .{ .expr = "string(duration('1h30m'))", .want = "5400s" },
            .{ .expr = "duration('1h') + duration('30m')", .want = "5400s" },
            .{ .expr = "timestamp('2009-02-13T23:31:30Z') + duration('30s')", .want = "timestamp(2009-02-13T23:32:00Z)" },
            .{ .expr = "timestamp('2009-02-13T23:31:30Z').getFullYear()", .want = "2009" },
            .{ .expr = "timestamp('2009-02-13T23:31:30Z').getMonth()", .want = "1" },
            .{ .expr = "timestamp('2009-02-13T23:31:30Z').getDate()", .want = "13" },
            .{ .expr = "timestamp('2009-02-13T23:31:30Z').getDayOfWeek()", .want = "5" },
            .{ .expr = "timestamp('2009-02-13T23:31:30Z').getHours()", .want = "23" },
            .{ .expr = "timestamp('2009-02-13T23:31:30Z').getHours('America/Los_Angeles')", .want = "15" },
            .{ .expr = "timestamp('2009-02-13T23:31:30Z').getHours('+11:00')", .want = "10" },
            .{ .expr = "duration('90m').getMinutes()", .want = "90" },
            .{ .expr = "duration('1.234s').getMilliseconds()", .want = "234" },
            .{ .expr = "timestamp('2009-02-13T23:31:30Z').getDayOfYear()", .want = "43" },
            .{ .expr = "type(timestamp('2009-02-13T23:31:30Z')) == google.protobuf.Timestamp", .want = "true" },
            .{ .expr = "type(duration('1s')) == google.protobuf.Duration", .want = "true" },
        });
    }

    test "misc" {
        try check(&.{
            .{ .expr = "'a' 'b'", .want = null }, // parse error
            .{ .expr = "{1: 'a', 1: 'b'}", .want = null }, // repeated key
            .{ .expr = "{[1]: 'a'}", .want = null }, // bad key type
            .{ .expr = "f_unknown(1)", .want = null }, // unbound function
            .{ .expr = "M{f: 1}", .want = null }, // message literal unsupported
        });
    }
};
