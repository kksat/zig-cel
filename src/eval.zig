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
const time = @import("time.zig");
const regex = @import("regex.zig");

pub const Value = value_mod.Value;
pub const Map = value_mod.Map;

pub const Error = error{ Eval, OutOfMemory };

pub const Binding = struct {
    name: []const u8,
    value: Value,
};

const Scope = struct {
    name: []const u8,
    value: Value,
};

pub const Evaluator = struct {
    alloc: std.mem.Allocator,
    tree: ast.Ast,
    vars: []const Binding = &.{},
    scopes: std.ArrayList(Scope) = .empty,
    err_msg: []const u8 = "",
    /// Set when the current error is an unbound-name error; used by select
    /// chains to attempt qualified type-name resolution.
    unbound: bool = false,

    const e = Evaluator;

    pub fn init(alloc: std.mem.Allocator, tree: ast.Ast) Evaluator {
        return .{ .alloc = alloc, .tree = tree };
    }

    pub fn run(ev: *Evaluator, vars: []const Binding) Error!Value {
        ev.vars = vars;
        ev.scopes.clearRetainingCapacity();
        return ev.eval(ev.tree.root);
    }

    fn fail(ev: *Evaluator, comptime fmt: []const u8, args: anytype) Error {
        ev.err_msg = std.fmt.allocPrint(ev.alloc, fmt, args) catch "out of memory";
        ev.unbound = false;
        return error.Eval;
    }

    fn failUnbound(ev: *Evaluator, name: []const u8) Error {
        ev.err_msg = std.fmt.allocPrint(ev.alloc, "unknown variable: undeclared reference to '{s}' (in container '')", .{name}) catch "out of memory";
        ev.unbound = true;
        return error.Eval;
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
                    else => return ev.noOverload("!_", &.{v}),
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
                    else => return ev.noOverload("-_", &.{v}),
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
                    else => return ev.noOverload("_?_:_", &.{cond}),
                }
            },
            .macro => return ev.evalMacro(node),
        }
    }

    fn noOverload(ev: *Evaluator, fn_name: []const u8, args: []const Value) Error {
        var aw = std.Io.Writer.Allocating.init(ev.alloc);
        const w = &aw.writer;
        for (args, 0..) |a, i| {
            if (i > 0) w.writeAll(", ") catch return error.OutOfMemory;
            w.writeAll(a.typeOf().name()) catch return error.OutOfMemory;
        }
        ev.err_msg = std.fmt.allocPrint(ev.alloc, "no_matching_overload: found no matching overload for '{s}' applied to '({s})'", .{ fn_name, aw.written() }) catch "out of memory";
        ev.unbound = false;
        return error.Eval;
    }

    // ---- identifiers and selection ----

    fn lookupVar(ev: *Evaluator, name: []const u8) ?Value {
        var i = ev.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, ev.scopes.items[i].name, name)) return ev.scopes.items[i].value;
        }
        for (ev.vars) |v| {
            if (std.mem.eql(u8, v.name, name)) return v.value;
        }
        return null;
    }

    /// Type names usable as bare identifier denotations. `dyn`,
    /// `duration`, and `timestamp` are functions, not denotations.
    fn bareTypeDenotation(name: []const u8) ?value_mod.Type {
        const map = .{
            .{ "null_type", value_mod.Type.null_type },
            .{ "bool", value_mod.Type.bool_ },
            .{ "int", value_mod.Type.int_ },
            .{ "uint", value_mod.Type.uint_ },
            .{ "double", value_mod.Type.double_ },
            .{ "string", value_mod.Type.string_ },
            .{ "bytes", value_mod.Type.bytes_ },
            .{ "list", value_mod.Type.list_ },
            .{ "map", value_mod.Type.map_ },
            .{ "type", value_mod.Type.type_ },
        };
        inline for (map) |entry| {
            if (std.mem.eql(u8, entry[0], name)) return entry[1];
        }
        return null;
    }

    fn evalIdent(ev: *Evaluator, node: ast.NodeIndex) Error!Value {
        const n = ev.tree.nodes[node];
        if (!n.flags.abs) {
            if (ev.lookupVar(n.payload.str)) |v| return v;
        } else if (ev.lookupVar(n.payload.str)) |v| {
            // Absolute names bypass comprehension scopes but still see
            // top-level bindings? No: `.a` resolves only in the root scope,
            // which for us is the top-level bindings.
            return v;
        }
        if (bareTypeDenotation(n.payload.str)) |t| return .{ .type_t = t };
        return ev.failUnbound(n.payload.str);
    }

    /// Collect the dotted name of a pure select chain rooted at an ident,
    /// e.g. `google.protobuf.Timestamp`. Returns null for anything else.
    fn qualifiedName(ev: *Evaluator, node: ast.NodeIndex) ?[]const u8 {
        var parts = std.ArrayList([]const u8){};
        defer parts.deinit(ev.alloc);
        var cur = node;
        while (true) {
            const n = ev.tree.nodes[cur];
            switch (n.kind) {
                .ident => {
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
        return out.toOwnedSlice(ev.alloc) catch null;
    }

    fn evalSelect(ev: *Evaluator, node: ast.NodeIndex) Error!Value {
        const n = ev.tree.nodes[node];
        const operand_node = ev.child(node);
        const operand = ev.eval(operand_node) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Eval => {
                // A select chain rooted at an unbound name may be a
                // well-known type denotation (google.protobuf.Timestamp).
                if (ev.unbound) {
                    if (ev.qualifiedName(node)) |qname| {
                        if (value_mod.Type.fromName(qname)) |t| {
                            return .{ .type_t = t };
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
        const lhs = try ev.tryEval(lhs_node);
        if (lhs) |v| {
            switch (v) {
                .bool_t => |b| {
                    if (b == absorbing) return .{ .bool_t = absorbing };
                },
                else => return ev.noOverload(if (op == .and_op) "_&&_" else "_||_", &.{v}),
            }
        }
        const rhs = try ev.tryEval(rhs_node);
        if (rhs) |v| {
            switch (v) {
                .bool_t => |b| {
                    if (b == absorbing) return .{ .bool_t = absorbing };
                    // rhs is non-absorbing: result is lhs if lhs was a bool.
                    if (lhs != null) return .{ .bool_t = !absorbing };
                    // lhs errored: error && true -> error; error || false -> error
                    return ev.fail("{s}", .{if (ev.err_msg.len > 0) ev.err_msg else "evaluation error"});
                },
                else => return ev.noOverload(if (op == .and_op) "_&&_" else "_||_", &.{v}),
            }
        }
        // Both errored (or rhs errored): propagate.
        return ev.fail("{s}", .{if (ev.err_msg.len > 0) ev.err_msg else "evaluation error"});
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
        return ev.dispatch(name, args.items);
    }

    fn dispatch(ev: *Evaluator, name: []const u8, args: []const Value) Error!Value {
        // Operators.
        if (std.mem.eql(u8, name, "_+_")) return ev.add(args);
        if (std.mem.eql(u8, name, "_-_")) return ev.sub(args);
        if (std.mem.eql(u8, name, "_*_")) return ev.mul(args);
        if (std.mem.eql(u8, name, "_/_")) return ev.div(args);
        if (std.mem.eql(u8, name, "_%_")) return ev.rem(args);
        if (std.mem.eql(u8, name, "_==_")) return .{ .bool_t = Value.eql(args[0], args[1]) };
        if (std.mem.eql(u8, name, "_!=_")) return .{ .bool_t = !Value.eql(args[0], args[1]) };
        if (std.mem.eql(u8, name, "_<_") or std.mem.eql(u8, name, "_<=_") or
            std.mem.eql(u8, name, "_>_") or std.mem.eql(u8, name, "_>=_"))
        {
            return ev.compare(name, args);
        }
        if (std.mem.eql(u8, name, "@in")) return ev.inOp(args);
        if (std.mem.eql(u8, name, "_[_]")) return ev.index(args);
        // Functions.
        if (std.mem.eql(u8, name, "size")) return ev.size(args);
        if (std.mem.eql(u8, name, "contains")) return ev.contains(args);
        if (std.mem.eql(u8, name, "startsWith")) return ev.startsEndsWith(args, true);
        if (std.mem.eql(u8, name, "endsWith")) return ev.startsEndsWith(args, false);
        if (std.mem.eql(u8, name, "matches")) return ev.matches(args);
        if (std.mem.eql(u8, name, "int")) return ev.toInt(args[0]);
        if (std.mem.eql(u8, name, "uint")) return ev.toUint(args[0]);
        if (std.mem.eql(u8, name, "double")) return ev.toDouble(args[0]);
        if (std.mem.eql(u8, name, "string")) return ev.toString(args[0]);
        if (std.mem.eql(u8, name, "bool")) return ev.toBool(args[0]);
        if (std.mem.eql(u8, name, "bytes")) return ev.toBytes(args[0]);
        if (std.mem.eql(u8, name, "type")) return .{ .type_t = args[0].typeOf() };
        if (std.mem.eql(u8, name, "dyn")) return args[0];
        if (std.mem.eql(u8, name, "duration")) return ev.toDuration(args[0]);
        if (std.mem.eql(u8, name, "timestamp")) return ev.toTimestamp(args[0]);
        if (std.mem.eql(u8, name, "getFullYear") or std.mem.eql(u8, name, "getMonth") or
            std.mem.eql(u8, name, "getDate") or std.mem.eql(u8, name, "getDayOfMonth") or
            std.mem.eql(u8, name, "getDayOfWeek") or std.mem.eql(u8, name, "getDayOfYear") or
            std.mem.eql(u8, name, "getHours") or std.mem.eql(u8, name, "getMinutes") or
            std.mem.eql(u8, name, "getSeconds") or std.mem.eql(u8, name, "getMilliseconds"))
        {
            return ev.getTimeField(name, args);
        }
        return ev.fail("unbound function: '{s}' is not defined", .{name});
    }

    // ---- arithmetic ----

    fn add(ev: *Evaluator, args: []const Value) Error!Value {
        const a = args[0];
        const b = args[1];
        switch (a) {
            .int_t => |x| {
                if (b != .int_t) return ev.noOverload("_+_", args);
                const r = @addWithOverflow(x, b.int_t);
                if (r[1] != 0) return ev.fail("return error for overflow: integer addition {d} + {d}", .{ x, b.int_t });
                return .{ .int_t = r[0] };
            },
            .uint_t => |x| {
                if (b != .uint_t) return ev.noOverload("_+_", args);
                const r = @addWithOverflow(x, b.uint_t);
                if (r[1] != 0) return ev.fail("return error for overflow: unsigned addition {d} + {d}", .{ x, b.uint_t });
                return .{ .uint_t = r[0] };
            },
            .double_t => |x| {
                if (b != .double_t) return ev.noOverload("_+_", args);
                return .{ .double_t = x + b.double_t };
            },
            .string_t => |x| {
                if (b != .string_t) return ev.noOverload("_+_", args);
                const out = try ev.alloc.alloc(u8, x.len + b.string_t.len);
                @memcpy(out[0..x.len], x);
                @memcpy(out[x.len..], b.string_t);
                return .{ .string_t = out };
            },
            .bytes_t => |x| {
                if (b != .bytes_t) return ev.noOverload("_+_", args);
                const out = try ev.alloc.alloc(u8, x.len + b.bytes_t.len);
                @memcpy(out[0..x.len], x);
                @memcpy(out[x.len..], b.bytes_t);
                return .{ .bytes_t = out };
            },
            .list_t => |x| {
                if (b != .list_t) return ev.noOverload("_+_", args);
                const y = b.list_t;
                const out = try ev.alloc.alloc(Value, x.len + y.len);
                @memcpy(out[0..x.len], x);
                @memcpy(out[x.len..], y);
                return .{ .list_t = out };
            },
            .duration_t => |x| {
                if (b == .duration_t) return ev.durationAdd(x, b.duration_t);
                if (b == .timestamp_t) return ev.timestampAddDuration(b.timestamp_t, x);
                return ev.noOverload("_+_", args);
            },
            .timestamp_t => |x| {
                if (b == .duration_t) return ev.timestampAddDuration(x, b.duration_t);
                return ev.noOverload("_+_", args);
            },
            else => return ev.noOverload("_+_", args),
        }
    }

    fn sub(ev: *Evaluator, args: []const Value) Error!Value {
        const a = args[0];
        const b = args[1];
        switch (a) {
            .int_t => |x| {
                if (b != .int_t) return ev.noOverload("_-_", args);
                const r = @subWithOverflow(x, b.int_t);
                if (r[1] != 0) return ev.fail("return error for overflow: integer subtraction {d} - {d}", .{ x, b.int_t });
                return .{ .int_t = r[0] };
            },
            .uint_t => |x| {
                if (b != .uint_t) return ev.noOverload("_-_", args);
                const r = @subWithOverflow(x, b.uint_t);
                if (r[1] != 0) return ev.fail("return error for overflow: unsigned subtraction {d} - {d}", .{ x, b.uint_t });
                return .{ .uint_t = r[0] };
            },
            .double_t => |x| {
                if (b != .double_t) return ev.noOverload("_-_", args);
                return .{ .double_t = x - b.double_t };
            },
            .duration_t => |x| {
                if (b == .duration_t) return ev.durationSub(x, b.duration_t);
                return ev.noOverload("_-_", args);
            },
            .timestamp_t => |x| {
                if (b == .timestamp_t) return ev.timestampSub(x, b.timestamp_t);
                if (b == .duration_t) return ev.timestampSubDuration(x, b.duration_t);
                return ev.noOverload("_-_", args);
            },
            else => return ev.noOverload("_-_", args),
        }
    }

    fn mul(ev: *Evaluator, args: []const Value) Error!Value {
        const a = args[0];
        const b = args[1];
        switch (a) {
            .int_t => |x| {
                if (b != .int_t) return ev.noOverload("_*_", args);
                const r = @mulWithOverflow(x, b.int_t);
                if (r[1] != 0) return ev.fail("return error for overflow: integer multiplication {d} * {d}", .{ x, b.int_t });
                return .{ .int_t = r[0] };
            },
            .uint_t => |x| {
                if (b != .uint_t) return ev.noOverload("_*_", args);
                const r = @mulWithOverflow(x, b.uint_t);
                if (r[1] != 0) return ev.fail("return error for overflow: unsigned multiplication {d} * {d}", .{ x, b.uint_t });
                return .{ .uint_t = r[0] };
            },
            .double_t => |x| {
                if (b != .double_t) return ev.noOverload("_*_", args);
                return .{ .double_t = x * b.double_t };
            },
            else => return ev.noOverload("_*_", args),
        }
    }

    fn div(ev: *Evaluator, args: []const Value) Error!Value {
        const a = args[0];
        const b = args[1];
        switch (a) {
            .int_t => |x| {
                if (b != .int_t) return ev.noOverload("_/_", args);
                const y = b.int_t;
                if (y == 0) return ev.fail("divide by zero (integer division by zero)", .{});
                if (x == std.math.minInt(i64) and y == -1) return ev.fail("return error for overflow: integer division overflow", .{});
                return .{ .int_t = @divTrunc(x, y) };
            },
            .uint_t => |x| {
                if (b != .uint_t) return ev.noOverload("_/_", args);
                const y = b.uint_t;
                if (y == 0) return ev.fail("divide by zero (unsigned division by zero)", .{});
                return .{ .uint_t = x / y };
            },
            .double_t => |x| {
                if (b != .double_t) return ev.noOverload("_/_", args);
                return .{ .double_t = x / b.double_t };
            },
            else => return ev.noOverload("_/_", args),
        }
    }

    fn rem(ev: *Evaluator, args: []const Value) Error!Value {
        const a = args[0];
        const b = args[1];
        switch (a) {
            .int_t => |x| {
                if (b != .int_t) return ev.noOverload("_%_", args);
                const y = b.int_t;
                if (y == 0) return ev.fail("modulus by zero", .{});
                if (x == std.math.minInt(i64) and y == -1) return ev.fail("return error for overflow: integer modulus overflow", .{});
                return .{ .int_t = @rem(x, y) };
            },
            .uint_t => |x| {
                if (b != .uint_t) return ev.noOverload("_%_", args);
                const y = b.uint_t;
                if (y == 0) return ev.fail("modulus by zero", .{});
                return .{ .uint_t = x % y };
            },
            else => return ev.noOverload("_%_", args),
        }
    }

    // ---- comparisons ----

    fn compare(ev: *Evaluator, name: []const u8, args: []const Value) Error!Value {
        const a = args[0];
        const b = args[1];
        // Type agreement (numeric cross-type allowed).
        const numeric_ok = a.isNumeric() and b.isNumeric();
        if (!numeric_ok and std.meta.activeTag(a) != std.meta.activeTag(b)) {
            return ev.noOverload(name, args);
        }
        // NaN makes every ordering comparison false.
        if (a == .double_t and std.math.isNan(a.double_t)) return .{ .bool_t = false };
        if (b == .double_t and std.math.isNan(b.double_t)) return .{ .bool_t = false };
        const order = Value.compare(a, b) orelse return ev.noOverload(name, args);
        const want_lt = std.mem.eql(u8, name, "_<_") or std.mem.eql(u8, name, "_<=_");
        const want_eq = std.mem.eql(u8, name, "_<=_") or std.mem.eql(u8, name, "_>=_");
        const result = switch (order) {
            .lt => want_lt,
            .gt => !want_lt,
            .eq => want_eq,
        };
        return .{ .bool_t = result };
    }

    fn inOp(ev: *Evaluator, args: []const Value) Error!Value {
        const needle = args[0];
        switch (args[1]) {
            .list_t => |l| {
                for (l) |item| {
                    if (Value.eql(needle, item)) return .{ .bool_t = true };
                }
                return .{ .bool_t = false };
            },
            .map_t => |m| return .{ .bool_t = m.has(needle) },
            else => return ev.noOverload("@in", args),
        }
    }

    fn index(ev: *Evaluator, args: []const Value) Error!Value {
        const container = args[0];
        const key = args[1];
        switch (container) {
            .list_t => |l| {
                // Integral numeric index (int, uint, or integral double).
                var idx: i64 = undefined;
                switch (key) {
                    .int_t => |i| idx = i,
                    .uint_t => |u| {
                        if (u > std.math.maxInt(i64)) return ev.fail("invalid_argument: index out of range: {d} (list size {d})", .{ u, l.len });
                        idx = @intCast(u);
                    },
                    .double_t => |d| {
                        if (std.math.isNan(d) or std.math.isInf(d) or @trunc(d) != d) {
                            return ev.fail("invalid_argument: list index must be an integer, got {d}", .{d});
                        }
                        if (d >= 9223372036854775808.0 or d < -9223372036854775808.0) {
                            return ev.fail("invalid_argument: index out of range: {d} (list size {d})", .{ d, l.len });
                        }
                        idx = @intFromFloat(d);
                    },
                    else => return ev.fail("invalid_argument: list index must be an integer, not {s}", .{key.typeOf().name()}),
                }
                if (idx < 0 or idx >= l.len) {
                    return ev.fail("invalid_argument: index out of range: {d} (list size {d})", .{ idx, l.len });
                }
                return l[@intCast(idx)];
            },
            .map_t => |m| {
                return m.get(key) orelse {
                    var aw = std.Io.Writer.Allocating.init(ev.alloc);
                    key.format(&aw.writer) catch return error.OutOfMemory;
                    return ev.fail("no such key: {s}", .{aw.written()});
                };
            },
            else => return ev.noOverload("_[_]", args),
        }
    }

    // ---- strings ----

    fn size(ev: *Evaluator, args: []const Value) Error!Value {
        switch (args[0]) {
            .string_t => |s| {
                var n: i64 = 0;
                var i: usize = 0;
                while (i < s.len) {
                    const l = std.unicode.utf8ByteSequenceLength(s[i]) catch {
                        return ev.fail("invalid UTF-8 in string", .{});
                    };
                    i += l;
                    n += 1;
                }
                return .{ .int_t = n };
            },
            .bytes_t => |s| return .{ .int_t = @intCast(s.len) },
            .list_t => |l| return .{ .int_t = @intCast(l.len) },
            .map_t => |m| return .{ .int_t = @intCast(m.size()) },
            else => return ev.noOverload("size", args),
        }
    }

    fn contains(ev: *Evaluator, args: []const Value) Error!Value {
        if (args[0] != .string_t or args[1] != .string_t) return ev.noOverload("contains", args);
        const hay = args[0].string_t;
        const needle = args[1].string_t;
        if (needle.len == 0) return .{ .bool_t = true };
        if (needle.len > hay.len) return .{ .bool_t = false };
        return .{ .bool_t = std.mem.indexOf(u8, hay, needle) != null };
    }

    fn startsEndsWith(ev: *Evaluator, args: []const Value, starts: bool) Error!Value {
        const fn_name = if (starts) "startsWith" else "endsWith";
        if (args[0] != .string_t or args[1] != .string_t) return ev.noOverload(fn_name, args);
        const s = args[0].string_t;
        const part = args[1].string_t;
        if (part.len > s.len) return .{ .bool_t = false };
        return .{ .bool_t = if (starts) std.mem.startsWith(u8, s, part) else std.mem.endsWith(u8, s, part) };
    }

    fn matches(ev: *Evaluator, args: []const Value) Error!Value {
        if (args[0] != .string_t or args[1] != .string_t) return ev.noOverload("matches", args);
        const input = args[0].string_t;
        const pattern = args[1].string_t;
        var re = regex.Regex.compile(ev.alloc, pattern) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return ev.fail("invalid regular expression: {s}", .{pattern}),
        };
        return .{ .bool_t = re.matches(input) };
    }

    // ---- conversions ----

    fn parseIntStr(ev: *Evaluator, s: []const u8, comptime T: type, fn_name: []const u8) Error!Value {
        if (s.len == 0) return ev.convErr(fn_name, "empty string");
        var i: usize = 0;
        var neg = false;
        if (s[0] == '-') {
            neg = true;
            i = 1;
        } else if (s[0] == '+') {
            i = 1;
        }
        if (i >= s.len) return ev.convErr(fn_name, "no digits");
        var mag: u64 = 0;
        while (i < s.len) : (i += 1) {
            const c = s[i];
            if (c < '0' or c > '9') return ev.convErr(fn_name, "invalid digit");
            mag = mag *% 10 +% (c - '0');
            if (mag > (1 << 63)) return ev.rangeErr(fn_name, "out of range");
        }
        if (T == i64) {
            if (neg) {
                if (mag > @as(u64, 1) << 63) return ev.rangeErr(fn_name, "out of range");
                if (mag == @as(u64, 1) << 63) return .{ .int_t = std.math.minInt(i64) };
                return .{ .int_t = -@as(i64, @intCast(mag)) };
            }
            if (mag > std.math.maxInt(i64)) return ev.rangeErr(fn_name, "out of range");
            return .{ .int_t = @intCast(mag) };
        } else {
            if (neg) return ev.rangeErr(fn_name, "negative value");
            return .{ .uint_t = mag };
        }
    }

    fn convErr(ev: *Evaluator, fn_name: []const u8, why: []const u8) Error {
        return ev.fail("Type conversion error: cannot convert to {s}: {s}", .{ fn_name, why });
    }

    fn rangeErr(ev: *Evaluator, fn_name: []const u8, why: []const u8) Error {
        return ev.fail("range error: {s}: {s}", .{ fn_name, why });
    }

    fn toInt(ev: *Evaluator, v: Value) Error!Value {
        switch (v) {
            .int_t => return v,
            .uint_t => |u| {
                if (u > std.math.maxInt(i64)) return ev.rangeErr("int", "uint value out of range");
                return .{ .int_t = @intCast(u) };
            },
            .double_t => |d| {
                if (std.math.isNan(d)) return ev.rangeErr("int", "NaN");
                const t = @trunc(d);
                if (t >= 9223372036854775808.0 or t < -9223372036854775808.0) return ev.rangeErr("int", "double value out of range");
                return .{ .int_t = @intFromFloat(t) };
            },
            .string_t => |s| return ev.parseIntStr(s, i64, "int"),
            .timestamp_t => |ts| return .{ .int_t = ts.seconds },
            else => return ev.noOverload("int", &.{v}),
        }
    }

    fn toUint(ev: *Evaluator, v: Value) Error!Value {
        switch (v) {
            .uint_t => return v,
            .int_t => |i| {
                if (i < 0) return ev.rangeErr("uint", "negative value");
                return .{ .uint_t = @intCast(i) };
            },
            .double_t => |d| {
                if (std.math.isNan(d)) return ev.rangeErr("uint", "NaN");
                const t = @trunc(d);
                if (t >= 18446744073709551616.0 or t < 0) return ev.rangeErr("uint", "double value out of range");
                return .{ .uint_t = @intFromFloat(t) };
            },
            .string_t => |s| return ev.parseIntStr(s, u64, "uint"),
            else => return ev.noOverload("uint", &.{v}),
        }
    }

    fn toDouble(ev: *Evaluator, v: Value) Error!Value {
        switch (v) {
            .double_t => return v,
            .int_t => |i| return .{ .double_t = @floatFromInt(i) },
            .uint_t => |u| return .{ .double_t = @floatFromInt(u) },
            .string_t => |s| {
                if (s.len == 0) return ev.convErr("double", "empty string");
                // Reject leading/trailing whitespace and hex (Go's
                // strconv.ParseFloat accepts "0x1p2"; CEL does not).
                for (s, 0..) |c, i| {
                    if (c == ' ' or c == '\t' or c == '\n' or c == '\r') return ev.convErr("double", "whitespace");
                    if ((c == 'x' or c == 'X') and i > 0 and s[0] == '0' and s.len > 1) return ev.convErr("double", "hex float");
                    if (c == 'p' or c == 'P') return ev.convErr("double", "hex float");
                }
                const d = std.fmt.parseFloat(f64, s) catch return ev.convErr("double", "invalid syntax");
                return .{ .double_t = d };
            },
            else => return ev.noOverload("double", &.{v}),
        }
    }

    fn toString(ev: *Evaluator, v: Value) Error!Value {
        switch (v) {
            .string_t => return v,
            .bool_t => |b| return .{ .string_t = if (b) "true" else "false" },
            .int_t => |i| return .{ .string_t = try std.fmt.allocPrint(ev.alloc, "{d}", .{i}) },
            .uint_t => |u| return .{ .string_t = try std.fmt.allocPrint(ev.alloc, "{d}", .{u}) },
            .double_t => |d| {
                var aw = std.Io.Writer.Allocating.init(ev.alloc);
                aw.writer.printFloat(d, .{ .mode = .decimal }) catch return error.OutOfMemory;
                return .{ .string_t = aw.toOwnedSlice() catch return error.OutOfMemory };
            },
            .bytes_t => |s| {
                if (!std.unicode.utf8ValidateSlice(s)) return ev.fail("invalid UTF-8 in bytes: cannot convert to string", .{});
                return .{ .string_t = s };
            },
            .timestamp_t => |ts| {
                const buf = time.formatTimestamp(ts);
                return .{ .string_t = try ev.alloc.dupe(u8, std.mem.sliceTo(&buf, 0)) };
            },
            .duration_t => |d| {
                var aw = std.Io.Writer.Allocating.init(ev.alloc);
                time.formatDuration(d, &aw.writer) catch return error.OutOfMemory;
                return .{ .string_t = aw.toOwnedSlice() catch return error.OutOfMemory };
            },
            else => return ev.noOverload("string", &.{v}),
        }
    }

    fn toBool(ev: *Evaluator, v: Value) Error!Value {
        switch (v) {
            .bool_t => return v,
            .string_t => |s| {
                const truthy = [_][]const u8{ "1", "t", "T", "true", "TRUE", "True" };
                const falsy = [_][]const u8{ "0", "f", "F", "false", "FALSE", "False" };
                for (truthy) |t| {
                    if (std.mem.eql(u8, s, t)) return .{ .bool_t = true };
                }
                for (falsy) |f| {
                    if (std.mem.eql(u8, s, f)) return .{ .bool_t = false };
                }
                return ev.convErr("bool", "invalid boolean literal");
            },
            else => return ev.noOverload("bool", &.{v}),
        }
    }

    fn toBytes(ev: *Evaluator, v: Value) Error!Value {
        switch (v) {
            .bytes_t => return v,
            .string_t => |s| return .{ .bytes_t = s },
            else => return ev.noOverload("bytes", &.{v}),
        }
    }

    fn toDuration(ev: *Evaluator, v: Value) Error!Value {
        switch (v) {
            .duration_t => return v,
            .string_t => |s| {
                const d = time.parseDuration(s) catch return ev.convErr("duration", "invalid duration string");
                return .{ .duration_t = d };
            },
            else => return ev.noOverload("duration", &.{v}),
        }
    }

    fn toTimestamp(ev: *Evaluator, v: Value) Error!Value {
        switch (v) {
            .timestamp_t => return v,
            .string_t => |s| {
                const t = time.parseTimestamp(s) catch return ev.convErr("timestamp", "invalid timestamp string");
                return .{ .timestamp_t = t };
            },
            else => return ev.noOverload("timestamp", &.{v}),
        }
    }

    // ---- duration/timestamp arithmetic ----

    fn durationAdd(ev: *Evaluator, a: value_mod.Duration, b: value_mod.Duration) Error!Value {
        const nanos = @as(i128, a.seconds) * 1_000_000_000 + a.nanos + @as(i128, b.seconds) * 1_000_000_000 + b.nanos;
        const d = time.durationFromNanos(nanos) catch return ev.rangeErr("duration addition", "out of range");
        return .{ .duration_t = d };
    }

    fn durationSub(ev: *Evaluator, a: value_mod.Duration, b: value_mod.Duration) Error!Value {
        const nanos = @as(i128, a.seconds) * 1_000_000_000 + a.nanos - (@as(i128, b.seconds) * 1_000_000_000 + b.nanos);
        const d = time.durationFromNanos(nanos) catch return ev.rangeErr("duration subtraction", "out of range");
        return .{ .duration_t = d };
    }

    fn timestampAddDuration(ev: *Evaluator, ts: value_mod.Timestamp, d: value_mod.Duration) Error!Value {
        const nanos = @as(i128, ts.seconds) * 1_000_000_000 + ts.nanos + @as(i128, d.seconds) * 1_000_000_000 + d.nanos;
        return ev.timestampFromNanos(nanos);
    }

    fn timestampSubDuration(ev: *Evaluator, ts: value_mod.Timestamp, d: value_mod.Duration) Error!Value {
        const nanos = @as(i128, ts.seconds) * 1_000_000_000 + ts.nanos - (@as(i128, d.seconds) * 1_000_000_000 + d.nanos);
        return ev.timestampFromNanos(nanos);
    }

    fn timestampFromNanos(ev: *Evaluator, nanos: i128) Error!Value {
        if (nanos < @as(i128, time.Timestamp.min_seconds) * 1_000_000_000 or
            nanos > @as(i128, time.Timestamp.max_seconds) * 1_000_000_000 + 999_999_999)
        {
            return ev.rangeErr("timestamp", "out of range");
        }
        const secs = @divFloor(nanos, 1_000_000_000);
        const ns = nanos - secs * 1_000_000_000;
        return .{ .timestamp_t = .{ .seconds = @intCast(secs), .nanos = @intCast(ns) } };
    }

    fn timestampSub(ev: *Evaluator, a: value_mod.Timestamp, b: value_mod.Timestamp) Error!Value {
        const nanos = @as(i128, a.seconds) * 1_000_000_000 + a.nanos - (@as(i128, b.seconds) * 1_000_000_000 + b.nanos);
        const d = time.durationFromNanos(nanos) catch return ev.rangeErr("timestamp subtraction", "out of range");
        return .{ .duration_t = d };
    }

    // ---- time field accessors ----

    fn getTimeField(ev: *Evaluator, name: []const u8, args: []const Value) Error!Value {
        const v = args[0];
        var tz = time.TimeZone{ .offset_secs = 0 };
        if (args.len == 2) {
            if (args[1] != .string_t) return ev.noOverload(name, args);
            const seconds: i64 = if (v == .timestamp_t) v.timestamp_t.seconds else 0;
            tz = time.TimeZone.resolve(args[1].string_t, seconds) catch return ev.convErr(name, "unknown timezone");
        } else if (args.len != 1) {
            return ev.noOverload(name, args);
        }

        if (v == .duration_t) {
            const d = v.duration_t;
            const total_nanos = time.durationToNanos(d);
            if (std.mem.eql(u8, name, "getHours")) return .{ .int_t = @intCast(@divTrunc(total_nanos, 3_600_000_000_000)) };
            if (std.mem.eql(u8, name, "getMinutes")) return .{ .int_t = @intCast(@divTrunc(total_nanos, 60_000_000_000)) };
            if (std.mem.eql(u8, name, "getSeconds")) return .{ .int_t = @intCast(@divTrunc(total_nanos, 1_000_000_000)) };
            if (std.mem.eql(u8, name, "getMilliseconds")) return .{ .int_t = @intCast(@divTrunc(@rem(total_nanos, 1_000_000_000), 1_000_000)) };
            return ev.noOverload(name, args);
        }
        if (v == .timestamp_t) {
            const parts = time.tsParts(v.timestamp_t, tz);
            if (std.mem.eql(u8, name, "getFullYear")) return .{ .int_t = parts.year };
            if (std.mem.eql(u8, name, "getMonth")) return .{ .int_t = @as(i64, parts.month) - 1 };
            if (std.mem.eql(u8, name, "getDate")) return .{ .int_t = @as(i64, parts.day) };
            if (std.mem.eql(u8, name, "getDayOfMonth")) return .{ .int_t = @as(i64, parts.day) - 1 };
            if (std.mem.eql(u8, name, "getDayOfWeek")) return .{ .int_t = @as(i64, parts.day_of_week) };
            if (std.mem.eql(u8, name, "getDayOfYear")) return .{ .int_t = parts.day_of_year };
            if (std.mem.eql(u8, name, "getHours")) return .{ .int_t = @as(i64, parts.hour) };
            if (std.mem.eql(u8, name, "getMinutes")) return .{ .int_t = @as(i64, parts.minute) };
            if (std.mem.eql(u8, name, "getSeconds")) return .{ .int_t = @as(i64, parts.second) };
            if (std.mem.eql(u8, name, "getMilliseconds")) return .{ .int_t = @as(i64, parts.millis) };
        }
        return ev.noOverload(name, args);
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
                                return ev.noOverload(if (is_all) "all" else "exists", &.{p});
                            },
                        }
                    } else {
                        saw_error = true;
                    }
                }
                if (saw_error) {
                    return ev.fail("{s}", .{if (ev.err_msg.len > 0) ev.err_msg else "evaluation error"});
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
                        else => return ev.noOverload("exists_one", &.{p}),
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
                        else => return ev.noOverload("filter", &.{p}),
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
                            else => return ev.noOverload("map", &.{p}),
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
                std.debug.print("eval error: {s} (expr: {s})\n", .{ ev.err_msg, src });
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
