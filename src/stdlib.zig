//! Standard function library shared by the tree-walking evaluator and the
//! bytecode VM: operator implementations, conversions, string/regex/time
//! functions, dispatched by interned `FnId` (no string comparisons on the
//! hot path).

const std = @import("std");
const value_mod = @import("value.zig");
const time = @import("time.zig");
const regex = @import("regex.zig");

pub const Value = value_mod.Value;
pub const Map = value_mod.Map;

/// A named variable binding supplied at evaluation time.
pub const Binding = struct {
    name: []const u8,
    value: Value,
};

pub const Error = error{ Eval, OutOfMemory };

/// Interned standard function/operator ids.
pub const FnId = enum(u8) {
    // Unary operators and macros (used for error messages; never
    // dispatched by name).
    not,
    neg,
    and_msg,
    or_msg,
    ternary,
    macro_all,
    macro_exists,
    macro_exists_one,
    macro_filter,
    macro_map,
    add,
    sub,
    mul,
    div,
    rem,
    eq,
    ne,
    lt,
    le,
    gt,
    ge,
    in,
    index,
    size,
    contains,
    starts_with,
    ends_with,
    matches,
    int,
    uint,
    double,
    string,
    bool,
    bytes,
    type,
    dyn,
    duration,
    timestamp,
    get_full_year,
    get_month,
    get_date,
    get_day_of_month,
    get_day_of_week,
    get_day_of_year,
    get_hours,
    get_minutes,
    get_seconds,
    get_milliseconds,
    unknown,
};

pub fn fnIdFromName(name: []const u8) FnId {
    const map = std.StaticStringMap(FnId).initComptime(.{
        .{ "_+_", FnId.add },        .{ "_-_", FnId.sub },
        .{ "_*_", FnId.mul },        .{ "_/_", FnId.div },
        .{ "_%_", FnId.rem },        .{ "_==_", FnId.eq },
        .{ "_!=_", FnId.ne },        .{ "_<_", FnId.lt },
        .{ "_<=_", FnId.le },        .{ "_>_", FnId.gt },
        .{ "_>=_", FnId.ge },        .{ "@in", FnId.in },
        .{ "_[_]", FnId.index },     .{ "size", FnId.size },
        .{ "contains", FnId.contains }, .{ "startsWith", FnId.starts_with },
        .{ "endsWith", FnId.ends_with }, .{ "matches", FnId.matches },
        .{ "int", FnId.int },        .{ "uint", FnId.uint },
        .{ "double", FnId.double },  .{ "string", FnId.string },
        .{ "bool", FnId.bool },      .{ "bytes", FnId.bytes },
        .{ "type", FnId.type },      .{ "dyn", FnId.dyn },
        .{ "duration", FnId.duration }, .{ "timestamp", FnId.timestamp },
        .{ "getFullYear", FnId.get_full_year }, .{ "getMonth", FnId.get_month },
        .{ "getDate", FnId.get_date }, .{ "getDayOfMonth", FnId.get_day_of_month },
        .{ "getDayOfWeek", FnId.get_day_of_week }, .{ "getDayOfYear", FnId.get_day_of_year },
        .{ "getHours", FnId.get_hours }, .{ "getMinutes", FnId.get_minutes },
        .{ "getSeconds", FnId.get_seconds }, .{ "getMilliseconds", FnId.get_milliseconds },
    });
    return map.get(name) orelse FnId.unknown;
}

/// The name for a function id (used in error messages).
pub fn fnName(id: FnId) []const u8 {
    return switch (id) {
        .add => "_+_",
        .sub => "_-_",
        .mul => "_*_",
        .div => "_/_",
        .rem => "_%_",
        .eq => "_==_",
        .ne => "_!=_",
        .lt => "_<_",
        .le => "_<=_",
        .gt => "_>_",
        .ge => "_>=_",
        .in => "@in",
        .index => "_[_]",
        .size => "size",
        .contains => "contains",
        .starts_with => "startsWith",
        .ends_with => "endsWith",
        .matches => "matches",
        .int => "int",
        .uint => "uint",
        .double => "double",
        .string => "string",
        .bool => "bool",
        .bytes => "bytes",
        .type => "type",
        .dyn => "dyn",
        .duration => "duration",
        .timestamp => "timestamp",
        .get_full_year => "getFullYear",
        .get_month => "getMonth",
        .get_date => "getDate",
        .get_day_of_month => "getDayOfMonth",
        .get_day_of_week => "getDayOfWeek",
        .get_day_of_year => "getDayOfYear",
        .get_hours => "getHours",
        .get_minutes => "getMinutes",
        .get_seconds => "getSeconds",
        .get_milliseconds => "getMilliseconds",
        .not => "!_",
        .neg => "-_",
        .and_msg => "_&&_",
        .or_msg => "_||_",
        .ternary => "_?_:_",
        .macro_all => "all",
        .macro_exists => "exists",
        .macro_exists_one => "exists_one",
        .macro_filter => "filter",
        .macro_map => "map",
        .unknown => "unknown",
    };
}

/// Evaluation context: arena allocator, last error, and a cache of compiled
/// regexes (program-owned when shared across runs).
pub const Ctx = struct {
    alloc: std.mem.Allocator,
    err_msg: []const u8 = "",
    unbound: bool = false,
    regexes: []*regex.Regex = &.{},

    pub fn init(alloc: std.mem.Allocator) Ctx {
        return .{ .alloc = alloc };
    }

    pub fn fail(c: *Ctx, comptime fmt: []const u8, args: anytype) Error {
        c.err_msg = std.fmt.allocPrint(c.alloc, fmt, args) catch "out of memory";
        c.unbound = false;
        return error.Eval;
    }

    pub fn failUnbound(c: *Ctx, name: []const u8) Error {
        c.err_msg = std.fmt.allocPrint(c.alloc, "unknown variable: undeclared reference to '{s}' (in container '')", .{name}) catch "out of memory";
        c.unbound = true;
        return error.Eval;
    }

    pub fn noOverload(c: *Ctx, id: FnId, args: []const Value) Error {
        var aw = std.Io.Writer.Allocating.init(c.alloc);
        const w = &aw.writer;
        for (args, 0..) |a, i| {
            if (i > 0) w.writeAll(", ") catch return error.OutOfMemory;
            w.writeAll(a.typeOf().name()) catch return error.OutOfMemory;
        }
        c.err_msg = std.fmt.allocPrint(c.alloc, "no_matching_overload: found no matching overload for '{s}' applied to '({s})'", .{ fnName(id), aw.written() }) catch "out of memory";
        c.unbound = false;
        return error.Eval;
    }
};

/// Type names usable as bare identifier denotations. `dyn`, `duration`,
/// and `timestamp` are functions, not denotations.
pub fn bareTypeDenotation(name: []const u8) ?value_mod.Type {
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

/// Compiled-regex cache lookup: reuse a program-owned compiled pattern when
/// the pattern slice matches, else compile into the run arena.
fn cachedRegex(c: *Ctx, pattern: []const u8) Error!*regex.Regex {
    for (c.regexes) |re| {
        if (std.mem.eql(u8, re.pattern, pattern)) return re;
    }
    const re = c.alloc.create(regex.Regex) catch return error.OutOfMemory;
    re.* = regex.Regex.compile(c.alloc, pattern) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return c.fail("invalid regular expression: {s}", .{pattern}),
    };
    return re;
}

pub fn call(c: *Ctx, id: FnId, args: []const Value) Error!Value {
    switch (id) {
        .not, .neg => {
            // Unary operators (also used by compile-time constant folding).
            const v = args[0];
            if (id == .not) {
                if (v == .bool_t) return .{ .bool_t = !v.bool_t };
                return c.noOverload(.not, args);
            }
            switch (v) {
                .int_t => |i| {
                    if (i == std.math.minInt(i64)) return c.fail("return error for overflow: negation of min int", .{});
                    return .{ .int_t = -i };
                },
                .double_t => |d| return .{ .double_t = -d },
                else => return c.noOverload(.neg, args),
            }
        },
        .and_msg, .or_msg, .ternary, .macro_all, .macro_exists, .macro_exists_one, .macro_filter, .macro_map => unreachable, // never dispatched
        .add => return add(c, args),
        .sub => return sub(c, args),
        .mul => return mul(c, args),
        .div => return div(c, args),
        .rem => return rem(c, args),
        .eq => return .{ .bool_t = Value.eql(args[0], args[1]) },
        .ne => return .{ .bool_t = !Value.eql(args[0], args[1]) },
        .lt, .le, .gt, .ge => return compare(c, id, args),
        .in => return inOp(c, args),
        .index => return index(c, args),
        .size => return size(c, args),
        .contains => return contains(c, args),
        .starts_with => return startsEndsWith(c, args, true),
        .ends_with => return startsEndsWith(c, args, false),
        .matches => return matches(c, args),
        .int => return toInt(c, args[0]),
        .uint => return toUint(c, args[0]),
        .double => return toDouble(c, args[0]),
        .string => return toString(c, args[0]),
        .bool => return toBool(c, args[0]),
        .bytes => return toBytes(c, args[0]),
        .type => return .{ .type_t = args[0].typeOf() },
        .dyn => return args[0],
        .duration => return toDuration(c, args[0]),
        .timestamp => return toTimestamp(c, args[0]),
        .get_full_year, .get_month, .get_date, .get_day_of_month, .get_day_of_week, .get_day_of_year, .get_hours, .get_minutes, .get_seconds, .get_milliseconds => return getTimeField(c, id, args),
        .unknown => return c.fail("unbound function: '{s}' is not defined", .{"<unknown>"}),
    }
}

pub fn callByName(c: *Ctx, name: []const u8, args: []const Value) Error!Value {
    const id = fnIdFromName(name);
    if (id == .unknown) {
        return c.fail("unbound function: '{s}' is not defined", .{name});
    }
    return call(c, id, args);
}

// ---- arithmetic ----

fn add(c: *Ctx, args: []const Value) Error!Value {
    const a = args[0];
    const b = args[1];
    switch (a) {
        .int_t => |x| {
            if (b != .int_t) return c.noOverload(.add, args);
            const r = @addWithOverflow(x, b.int_t);
            if (r[1] != 0) return c.fail("return error for overflow: integer addition {d} + {d}", .{ x, b.int_t });
            return .{ .int_t = r[0] };
        },
        .uint_t => |x| {
            if (b != .uint_t) return c.noOverload(.add, args);
            const r = @addWithOverflow(x, b.uint_t);
            if (r[1] != 0) return c.fail("return error for overflow: unsigned addition {d} + {d}", .{ x, b.uint_t });
            return .{ .uint_t = r[0] };
        },
        .double_t => |x| {
            if (b != .double_t) return c.noOverload(.add, args);
            return .{ .double_t = x + b.double_t };
        },
        .string_t => |x| {
            if (b != .string_t) return c.noOverload(.add, args);
            const out = try c.alloc.alloc(u8, x.len + b.string_t.len);
            @memcpy(out[0..x.len], x);
            @memcpy(out[x.len..], b.string_t);
            return .{ .string_t = out };
        },
        .bytes_t => |x| {
            if (b != .bytes_t) return c.noOverload(.add, args);
            const out = try c.alloc.alloc(u8, x.len + b.bytes_t.len);
            @memcpy(out[0..x.len], x);
            @memcpy(out[x.len..], b.bytes_t);
            return .{ .bytes_t = out };
        },
        .list_t => |x| {
            if (b != .list_t) return c.noOverload(.add, args);
            const y = b.list_t;
            const out = try c.alloc.alloc(Value, x.len + y.len);
            @memcpy(out[0..x.len], x);
            @memcpy(out[x.len..], y);
            return .{ .list_t = out };
        },
        .duration_t => |x| {
            if (b == .duration_t) return durationAdd(c, x, b.duration_t);
            if (b == .timestamp_t) return timestampAddDuration(c, b.timestamp_t, x);
            return c.noOverload(.add, args);
        },
        .timestamp_t => |x| {
            if (b == .duration_t) return timestampAddDuration(c, x, b.duration_t);
            return c.noOverload(.add, args);
        },
        else => return c.noOverload(.add, args),
    }
}

fn sub(c: *Ctx, args: []const Value) Error!Value {
    const a = args[0];
    const b = args[1];
    switch (a) {
        .int_t => |x| {
            if (b != .int_t) return c.noOverload(.sub, args);
            const r = @subWithOverflow(x, b.int_t);
            if (r[1] != 0) return c.fail("return error for overflow: integer subtraction {d} - {d}", .{ x, b.int_t });
            return .{ .int_t = r[0] };
        },
        .uint_t => |x| {
            if (b != .uint_t) return c.noOverload(.sub, args);
            const r = @subWithOverflow(x, b.uint_t);
            if (r[1] != 0) return c.fail("return error for overflow: unsigned subtraction {d} - {d}", .{ x, b.uint_t });
            return .{ .uint_t = r[0] };
        },
        .double_t => |x| {
            if (b != .double_t) return c.noOverload(.sub, args);
            return .{ .double_t = x - b.double_t };
        },
        .duration_t => |x| {
            if (b == .duration_t) return durationSub(c, x, b.duration_t);
            return c.noOverload(.sub, args);
        },
        .timestamp_t => |x| {
            if (b == .timestamp_t) return timestampSub(c, x, b.timestamp_t);
            if (b == .duration_t) return timestampSubDuration(c, x, b.duration_t);
            return c.noOverload(.sub, args);
        },
        else => return c.noOverload(.sub, args),
    }
}

fn mul(c: *Ctx, args: []const Value) Error!Value {
    const a = args[0];
    const b = args[1];
    switch (a) {
        .int_t => |x| {
            if (b != .int_t) return c.noOverload(.mul, args);
            const r = @mulWithOverflow(x, b.int_t);
            if (r[1] != 0) return c.fail("return error for overflow: integer multiplication {d} * {d}", .{ x, b.int_t });
            return .{ .int_t = r[0] };
        },
        .uint_t => |x| {
            if (b != .uint_t) return c.noOverload(.mul, args);
            const r = @mulWithOverflow(x, b.uint_t);
            if (r[1] != 0) return c.fail("return error for overflow: unsigned multiplication {d} * {d}", .{ x, b.uint_t });
            return .{ .uint_t = r[0] };
        },
        .double_t => |x| {
            if (b != .double_t) return c.noOverload(.mul, args);
            return .{ .double_t = x * b.double_t };
        },
        else => return c.noOverload(.mul, args),
    }
}

fn div(c: *Ctx, args: []const Value) Error!Value {
    const a = args[0];
    const b = args[1];
    switch (a) {
        .int_t => |x| {
            if (b != .int_t) return c.noOverload(.div, args);
            const y = b.int_t;
            if (y == 0) return c.fail("divide by zero (integer division by zero)", .{});
            if (x == std.math.minInt(i64) and y == -1) return c.fail("return error for overflow: integer division overflow", .{});
            return .{ .int_t = @divTrunc(x, y) };
        },
        .uint_t => |x| {
            if (b != .uint_t) return c.noOverload(.div, args);
            const y = b.uint_t;
            if (y == 0) return c.fail("divide by zero (unsigned division by zero)", .{});
            return .{ .uint_t = x / y };
        },
        .double_t => |x| {
            if (b != .double_t) return c.noOverload(.div, args);
            return .{ .double_t = x / b.double_t };
        },
        else => return c.noOverload(.div, args),
    }
}

fn rem(c: *Ctx, args: []const Value) Error!Value {
    const a = args[0];
    const b = args[1];
    switch (a) {
        .int_t => |x| {
            if (b != .int_t) return c.noOverload(.rem, args);
            const y = b.int_t;
            if (y == 0) return c.fail("modulus by zero", .{});
            if (x == std.math.minInt(i64) and y == -1) return c.fail("return error for overflow: integer modulus overflow", .{});
            return .{ .int_t = @rem(x, y) };
        },
        .uint_t => |x| {
            if (b != .uint_t) return c.noOverload(.rem, args);
            const y = b.uint_t;
            if (y == 0) return c.fail("modulus by zero", .{});
            return .{ .uint_t = x % y };
        },
        else => return c.noOverload(.rem, args),
    }
}

// ---- comparisons ----

fn compare(c: *Ctx, id: FnId, args: []const Value) Error!Value {
    const a = args[0];
    const b = args[1];
    // Type agreement (numeric cross-type allowed).
    const numeric_ok = a.isNumeric() and b.isNumeric();
    if (!numeric_ok and std.meta.activeTag(a) != std.meta.activeTag(b)) {
        return c.noOverload(id, args);
    }
    // NaN makes every ordering comparison false.
    if (a == .double_t and std.math.isNan(a.double_t)) return .{ .bool_t = false };
    if (b == .double_t and std.math.isNan(b.double_t)) return .{ .bool_t = false };
    const order = Value.compare(a, b) orelse return c.noOverload(id, args);
    const want_lt = id == .lt or id == .le;
    const want_eq = id == .le or id == .ge;
    const result = switch (order) {
        .lt => want_lt,
        .gt => !want_lt,
        .eq => want_eq,
    };
    return .{ .bool_t = result };
}

fn inOp(c: *Ctx, args: []const Value) Error!Value {
    const needle = args[0];
    switch (args[1]) {
        .list_t => |l| {
            for (l) |item| {
                if (Value.eql(needle, item)) return .{ .bool_t = true };
            }
            return .{ .bool_t = false };
        },
        .map_t => |m| return .{ .bool_t = m.has(needle) },
        else => return c.noOverload(.in, args),
    }
}

fn index(c: *Ctx, args: []const Value) Error!Value {
    const container = args[0];
    const key = args[1];
    switch (container) {
        .list_t => |l| {
            // Integral numeric index (int, uint, or integral double).
            var idx: i64 = undefined;
            switch (key) {
                .int_t => |i| idx = i,
                .uint_t => |u| {
                    if (u > std.math.maxInt(i64)) return c.fail("invalid_argument: index out of range: {d} (list size {d})", .{ u, l.len });
                    idx = @intCast(u);
                },
                .double_t => |d| {
                    if (std.math.isNan(d) or std.math.isInf(d) or @trunc(d) != d) {
                        return c.fail("invalid_argument: list index must be an integer, got {d}", .{d});
                    }
                    if (d >= 9223372036854775808.0 or d < -9223372036854775808.0) {
                        return c.fail("invalid_argument: index out of range: {d} (list size {d})", .{ d, l.len });
                    }
                    idx = @intFromFloat(d);
                },
                else => return c.fail("invalid_argument: list index must be an integer, not {s}", .{key.typeOf().name()}),
            }
            if (idx < 0 or idx >= l.len) {
                return c.fail("invalid_argument: index out of range: {d} (list size {d})", .{ idx, l.len });
            }
            return l[@intCast(idx)];
        },
        .map_t => |m| {
            return m.get(key) orelse {
                var aw = std.Io.Writer.Allocating.init(c.alloc);
                key.format(&aw.writer) catch return error.OutOfMemory;
                return c.fail("no such key: {s}", .{aw.written()});
            };
        },
        else => return c.noOverload(.index, args),
    }
}

// ---- strings ----

fn size(c: *Ctx, args: []const Value) Error!Value {
    switch (args[0]) {
        .string_t => |s| {
            var n: i64 = 0;
            var i: usize = 0;
            while (i < s.len) {
                const l = std.unicode.utf8ByteSequenceLength(s[i]) catch {
                    return c.fail("invalid UTF-8 in string", .{});
                };
                i += l;
                n += 1;
            }
            return .{ .int_t = n };
        },
        .bytes_t => |s| return .{ .int_t = @intCast(s.len) },
        .list_t => |l| return .{ .int_t = @intCast(l.len) },
        .map_t => |m| return .{ .int_t = @intCast(m.size()) },
        else => return c.noOverload(.size, args),
    }
}

fn contains(c: *Ctx, args: []const Value) Error!Value {
    if (args[0] != .string_t or args[1] != .string_t) return c.noOverload(.contains, args);
    const hay = args[0].string_t;
    const needle = args[1].string_t;
    if (needle.len == 0) return .{ .bool_t = true };
    if (needle.len > hay.len) return .{ .bool_t = false };
    return .{ .bool_t = std.mem.indexOf(u8, hay, needle) != null };
}

fn startsEndsWith(c: *Ctx, args: []const Value, starts: bool) Error!Value {
    if (args[0] != .string_t or args[1] != .string_t) return c.noOverload(if (starts) .starts_with else .ends_with, args);
    const s = args[0].string_t;
    const part = args[1].string_t;
    if (part.len > s.len) return .{ .bool_t = false };
    return .{ .bool_t = if (starts) std.mem.startsWith(u8, s, part) else std.mem.endsWith(u8, s, part) };
}

fn matches(c: *Ctx, args: []const Value) Error!Value {
    if (args[0] != .string_t or args[1] != .string_t) return c.noOverload(.matches, args);
    const input = args[0].string_t;
    const pattern = args[1].string_t;
    var re = regex.Regex.compile(c.alloc, pattern) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return c.fail("invalid regular expression: {s}", .{pattern}),
    };
    return .{ .bool_t = re.matches(input) };
}

// ---- conversions ----

fn parseIntStr(c: *Ctx, s: []const u8, comptime T: type, fn_name: []const u8) Error!Value {
    if (s.len == 0) return convErr(c, fn_name, "empty string");
    var i: usize = 0;
    var neg = false;
    if (s[0] == '-') {
        neg = true;
        i = 1;
    } else if (s[0] == '+') {
        i = 1;
    }
    if (i >= s.len) return convErr(c, fn_name, "no digits");
    var mag: u64 = 0;
    while (i < s.len) : (i += 1) {
        const ch = s[i];
        if (ch < '0' or ch > '9') return convErr(c, fn_name, "invalid digit");
        mag = mag *% 10 +% (ch - '0');
        if (mag > (1 << 63)) return rangeErr(c, fn_name, "out of range");
    }
    if (T == i64) {
        if (neg) {
            if (mag > @as(u64, 1) << 63) return rangeErr(c, fn_name, "out of range");
            if (mag == @as(u64, 1) << 63) return .{ .int_t = std.math.minInt(i64) };
            return .{ .int_t = -@as(i64, @intCast(mag)) };
        }
        if (mag > std.math.maxInt(i64)) return rangeErr(c, fn_name, "out of range");
        return .{ .int_t = @intCast(mag) };
    } else {
        if (neg) return rangeErr(c, fn_name, "negative value");
        return .{ .uint_t = mag };
    }
}

fn convErr(c: *Ctx, fn_name: []const u8, why: []const u8) Error {
    return c.fail("Type conversion error: cannot convert to {s}: {s}", .{ fn_name, why });
}

fn rangeErr(c: *Ctx, fn_name: []const u8, why: []const u8) Error {
    return c.fail("range error: {s}: {s}", .{ fn_name, why });
}

fn toInt(c: *Ctx, v: Value) Error!Value {
    switch (v) {
        .int_t => return v,
        .uint_t => |u| {
            if (u > std.math.maxInt(i64)) return rangeErr(c, "int", "uint value out of range");
            return .{ .int_t = @intCast(u) };
        },
        .double_t => |d| {
            if (std.math.isNan(d)) return rangeErr(c, "int", "NaN");
            const t = @trunc(d);
            // The range is (minInt, maxInt) non-inclusive at both ends.
            if (t >= 9223372036854775808.0 or t <= -9223372036854775808.0) return rangeErr(c, "int", "double value out of range");
            return .{ .int_t = @intFromFloat(t) };
        },
        .string_t => |s| return parseIntStr(c, s, i64, "int"),
        .timestamp_t => |ts| return .{ .int_t = ts.seconds },
        else => return c.noOverload(.int, &.{v}),
    }
}

fn toUint(c: *Ctx, v: Value) Error!Value {
    switch (v) {
        .uint_t => return v,
        .int_t => |i| {
            if (i < 0) return rangeErr(c, "uint", "negative value");
            return .{ .uint_t = @intCast(i) };
        },
        .double_t => |d| {
            if (std.math.isNan(d)) return rangeErr(c, "uint", "NaN");
            const t = @trunc(d);
            if (t >= 18446744073709551616.0 or t < 0) return rangeErr(c, "uint", "double value out of range");
            return .{ .uint_t = @intFromFloat(t) };
        },
        .string_t => |s| return parseIntStr(c, s, u64, "uint"),
        else => return c.noOverload(.uint, &.{v}),
    }
}

fn toDouble(c: *Ctx, v: Value) Error!Value {
    switch (v) {
        .double_t => return v,
        .int_t => |i| return .{ .double_t = @floatFromInt(i) },
        .uint_t => |u| return .{ .double_t = @floatFromInt(u) },
        .string_t => |s| {
            if (s.len == 0) return convErr(c, "double", "empty string");
            // Reject leading/trailing whitespace and hex (Go's
            // strconv.ParseFloat accepts "0x1p2"; CEL does not).
            for (s, 0..) |ch, i| {
                if (ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r') return convErr(c, "double", "whitespace");
                if ((ch == 'x' or ch == 'X') and i > 0 and s[0] == '0' and s.len > 1) return convErr(c, "double", "hex float");
                if (ch == 'p' or ch == 'P') return convErr(c, "double", "hex float");
            }
            const d = std.fmt.parseFloat(f64, s) catch return convErr(c, "double", "invalid syntax");
            return .{ .double_t = d };
        },
        else => return c.noOverload(.double, &.{v}),
    }
}

fn toString(c: *Ctx, v: Value) Error!Value {
    switch (v) {
        .string_t => return v,
        .bool_t => |b| return .{ .string_t = if (b) "true" else "false" },
        .int_t => |i| return .{ .string_t = try std.fmt.allocPrint(c.alloc, "{d}", .{i}) },
        .uint_t => |u| return .{ .string_t = try std.fmt.allocPrint(c.alloc, "{d}", .{u}) },
        .double_t => |d| {
            var aw = std.Io.Writer.Allocating.init(c.alloc);
            aw.writer.printFloat(d, .{ .mode = .decimal }) catch return error.OutOfMemory;
            return .{ .string_t = aw.toOwnedSlice() catch return error.OutOfMemory };
        },
        .bytes_t => |s| {
            if (!std.unicode.utf8ValidateSlice(s)) return c.fail("invalid UTF-8 in bytes: cannot convert to string", .{});
            return .{ .string_t = s };
        },
        .timestamp_t => |ts| {
            const buf = time.formatTimestamp(ts);
            return .{ .string_t = try c.alloc.dupe(u8, std.mem.sliceTo(&buf, 0)) };
        },
        .duration_t => |d| {
            var aw = std.Io.Writer.Allocating.init(c.alloc);
            time.formatDuration(d, &aw.writer) catch return error.OutOfMemory;
            return .{ .string_t = aw.toOwnedSlice() catch return error.OutOfMemory };
        },
        else => return c.noOverload(.string, &.{v}),
    }
}

fn toBool(c: *Ctx, v: Value) Error!Value {
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
            return convErr(c, "bool", "invalid boolean literal");
        },
        else => return c.noOverload(.bool, &.{v}),
    }
}

fn toBytes(c: *Ctx, v: Value) Error!Value {
    switch (v) {
        .bytes_t => return v,
        .string_t => |s| return .{ .bytes_t = s },
        else => return c.noOverload(.bytes, &.{v}),
    }
}

fn toDuration(c: *Ctx, v: Value) Error!Value {
    switch (v) {
        .duration_t => return v,
        .string_t => |s| {
            const d = time.parseDuration(s) catch return convErr(c, "duration", "invalid duration string");
            return .{ .duration_t = d };
        },
        else => return c.noOverload(.duration, &.{v}),
    }
}

fn toTimestamp(c: *Ctx, v: Value) Error!Value {
    switch (v) {
        .timestamp_t => return v,
        .int_t => |i| {
            if (i < time.Timestamp.min_seconds or i > time.Timestamp.max_seconds) {
                return rangeErr(c, "timestamp", "seconds out of range");
            }
            return .{ .timestamp_t = .{ .seconds = i, .nanos = 0 } };
        },
        .string_t => |s| {
            const t = time.parseTimestamp(s) catch return convErr(c, "timestamp", "invalid timestamp string");
            return .{ .timestamp_t = t };
        },
        else => return c.noOverload(.timestamp, &.{v}),
    }
}

// ---- duration/timestamp arithmetic ----

fn durationAdd(c: *Ctx, a: value_mod.Duration, b: value_mod.Duration) Error!Value {
    const nanos = @as(i128, a.seconds) * 1_000_000_000 + a.nanos + @as(i128, b.seconds) * 1_000_000_000 + b.nanos;
    const d = time.durationFromNanos(nanos) catch return rangeErr(c, "duration addition", "out of range");
    return .{ .duration_t = d };
}

fn durationSub(c: *Ctx, a: value_mod.Duration, b: value_mod.Duration) Error!Value {
    const nanos = @as(i128, a.seconds) * 1_000_000_000 + a.nanos - (@as(i128, b.seconds) * 1_000_000_000 + b.nanos);
    const d = time.durationFromNanos(nanos) catch return rangeErr(c, "duration subtraction", "out of range");
    return .{ .duration_t = d };
}

fn timestampAddDuration(c: *Ctx, ts: value_mod.Timestamp, d: value_mod.Duration) Error!Value {
    const nanos = @as(i128, ts.seconds) * 1_000_000_000 + ts.nanos + @as(i128, d.seconds) * 1_000_000_000 + d.nanos;
    return timestampFromNanos(c, nanos);
}

fn timestampSubDuration(c: *Ctx, ts: value_mod.Timestamp, d: value_mod.Duration) Error!Value {
    const nanos = @as(i128, ts.seconds) * 1_000_000_000 + ts.nanos - (@as(i128, d.seconds) * 1_000_000_000 + d.nanos);
    return timestampFromNanos(c, nanos);
}

fn timestampFromNanos(c: *Ctx, nanos: i128) Error!Value {
    if (nanos < @as(i128, time.Timestamp.min_seconds) * 1_000_000_000 or
        nanos > @as(i128, time.Timestamp.max_seconds) * 1_000_000_000 + 999_999_999)
    {
        return rangeErr(c, "timestamp", "out of range");
    }
    const secs = @divFloor(nanos, 1_000_000_000);
    const ns = nanos - secs * 1_000_000_000;
    return .{ .timestamp_t = .{ .seconds = @intCast(secs), .nanos = @intCast(ns) } };
}

fn timestampSub(c: *Ctx, a: value_mod.Timestamp, b: value_mod.Timestamp) Error!Value {
    const nanos = @as(i128, a.seconds) * 1_000_000_000 + a.nanos - (@as(i128, b.seconds) * 1_000_000_000 + b.nanos);
    const d = time.durationFromNanos(nanos) catch return rangeErr(c, "timestamp subtraction", "out of range");
    return .{ .duration_t = d };
}

// ---- time field accessors ----

fn getTimeField(c: *Ctx, id: FnId, args: []const Value) Error!Value {
    const v = args[0];
    var tz = time.TimeZone{ .offset_secs = 0 };
    if (args.len == 2) {
        if (args[1] != .string_t) return c.noOverload(id, args);
        const seconds: i64 = if (v == .timestamp_t) v.timestamp_t.seconds else 0;
        tz = time.TimeZone.resolve(args[1].string_t, seconds) catch return convErr(c, fnName(id), "unknown timezone");
    } else if (args.len != 1) {
        return c.noOverload(id, args);
    }

    if (v == .duration_t) {
        const d = v.duration_t;
        const total_nanos = time.durationToNanos(d);
        if (id == .get_hours) return .{ .int_t = @intCast(@divTrunc(total_nanos, 3_600_000_000_000)) };
        if (id == .get_minutes) return .{ .int_t = @intCast(@divTrunc(total_nanos, 60_000_000_000)) };
        if (id == .get_seconds) return .{ .int_t = @intCast(@divTrunc(total_nanos, 1_000_000_000)) };
        if (id == .get_milliseconds) return .{ .int_t = @intCast(@divTrunc(@rem(total_nanos, 1_000_000_000), 1_000_000)) };
        return c.noOverload(id, args);
    }
    if (v == .timestamp_t) {
        const parts = time.tsParts(v.timestamp_t, tz);
        if (id == .get_full_year) return .{ .int_t = parts.year };
        if (id == .get_month) return .{ .int_t = @as(i64, parts.month) - 1 };
        if (id == .get_date) return .{ .int_t = @as(i64, parts.day) };
        if (id == .get_day_of_month) return .{ .int_t = @as(i64, parts.day) - 1 };
        if (id == .get_day_of_week) return .{ .int_t = @as(i64, parts.day_of_week) };
        if (id == .get_day_of_year) return .{ .int_t = parts.day_of_year };
        if (id == .get_hours) return .{ .int_t = @as(i64, parts.hour) };
        if (id == .get_minutes) return .{ .int_t = @as(i64, parts.minute) };
        if (id == .get_seconds) return .{ .int_t = @as(i64, parts.second) };
        if (id == .get_milliseconds) return .{ .int_t = @as(i64, parts.millis) };
    }
    return c.noOverload(id, args);
}


