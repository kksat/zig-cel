//! CEL extension functions: `math` and `strings`
//! (https://github.com/google/cel-go/blob/main/ext/README.md).
//!
//! Both extensions are opt-in, like cel-go: enable them at parse time (for
//! the `math.greatest`/`math.least` macros and qualified function names
//! such as `math.ceil` and `strings.quote`) and at evaluation time (the
//! function dispatch only resolves extension functions when enabled).
//!
//! Semantics follow cel-go's ext package, including:
//! - `math.least`/`math.greatest` var-arg macros expanding to
//!   `math.@min`/`math.@max` (pair, identity, and list forms with
//!   heterogeneous numeric comparison);
//! - bitwise functions with cel-go/Go shift semantics (shifts >= 64 are 0);
//! - string functions operating on Unicode code points;
//! - `format` with the v2 formatting rules: unquoted strings inside
//!   lists/maps, maps sorted by formatted key, and Go-`strconv`-compatible
//!   `%.Nf` / `%.Ne` rounding via exact decimal expansion of the double.

const std = @import("std");
const value_mod = @import("value.zig");
const time = @import("time.zig");
const stdlib = @import("stdlib.zig");

pub const Value = value_mod.Value;
pub const Map = value_mod.Map;
pub const Ctx = stdlib.Ctx;
pub const Error = stdlib.Error;
pub const FnId = stdlib.FnId;

/// Which extensions are enabled. Off by default (opt-in, like cel-go).
pub const Extensions = packed struct(u8) {
    math: bool = false,
    strings: bool = false,
    _pad: u6 = 0,

    pub const none: Extensions = .{};
    pub const math_only: Extensions = .{ .math = true };
    pub const strings_only: Extensions = .{ .strings = true };
    pub const all: Extensions = .{ .math = true, .strings = true };
};

/// Whether a function id belongs to the math extension.
pub fn isMathFn(id: FnId) bool {
    return switch (id) {
        .math_min, .math_max, .math_ceil, .math_floor, .math_round, .math_trunc, .math_abs, .math_sign, .math_is_inf, .math_is_nan, .math_is_finite, .math_sqrt, .math_bit_and, .math_bit_or, .math_bit_xor, .math_bit_not, .math_bit_shift_left, .math_bit_shift_right => true,
        else => false,
    };
}

/// Whether a function id belongs to the strings extension.
pub fn isStringsFn(id: FnId) bool {
    return switch (id) {
        .ext_char_at, .ext_index_of, .ext_last_index_of, .ext_lower_ascii, .ext_upper_ascii, .ext_replace, .ext_split, .ext_substring, .ext_trim, .ext_join, .ext_strings_quote, .ext_reverse, .ext_format => true,
        else => false,
    };
}

/// True when the function id is available under the enabled extensions.
pub fn available(ext: Extensions, id: FnId) bool {
    if (isMathFn(id)) return ext.math;
    if (isStringsFn(id)) return ext.strings;
    return true;
}

pub fn call(c: *Ctx, id: FnId, args: []const Value) Error!Value {
    switch (id) {
        // ---- math ----
        .math_min => return minMax(c, args, true),
        .math_max => return minMax(c, args, false),
        .math_ceil => return roundOp(c, args, .ceil),
        .math_floor => return roundOp(c, args, .floor),
        .math_round => return roundOp(c, args, .round),
        .math_trunc => return roundOp(c, args, .trunc),
        .math_abs => return abs(c, args),
        .math_sign => return sign(c, args),
        .math_is_inf => return isInfFn(c, args),
        .math_is_nan => return isNanFn(c, args),
        .math_is_finite => return isFiniteFn(c, args),
        .math_sqrt => return sqrt(c, args),
        .math_bit_and => return bitOp(c, args, .bit_and),
        .math_bit_or => return bitOp(c, args, .bit_or),
        .math_bit_xor => return bitOp(c, args, .bit_xor),
        .math_bit_not => return bitNot(c, args),
        .math_bit_shift_left => return bitShift(c, args, true),
        .math_bit_shift_right => return bitShift(c, args, false),

        // ---- strings ----
        .ext_char_at => return charAt(c, args),
        .ext_index_of => return indexOf(c, args, false),
        .ext_last_index_of => return indexOf(c, args, true),
        .ext_lower_ascii => return asciiCase(c, args, false),
        .ext_upper_ascii => return asciiCase(c, args, true),
        .ext_replace => return replace(c, args),
        .ext_split => return split(c, args),
        .ext_substring => return substring(c, args),
        .ext_trim => return trim(c, args),
        .ext_join => return join(c, args),
        .ext_strings_quote => return quote(c, args),
        .ext_reverse => return reverse(c, args),
        .ext_format => return format(c, args),

        else => return c.fail("unbound function: '{s}' is not defined", .{stdlib.fnName(id)}),
    }
}

// ---- math extension ----

/// Compare two numeric values for min/max: cross-type comparison follows
/// cel-go's Compare (int cast to double at the extremes); NaN compares
/// equal to everything (cel-go's compareDouble returns 0 for NaN).
fn extCompare(a: Value, b: Value) std.math.Order {
    if (a == .double_t and b == .double_t) {
        if (std.math.isNan(a.double_t) or std.math.isNan(b.double_t)) return .eq;
        return std.math.order(a.double_t, b.double_t);
    }
    if (Value.compare(a, b)) |o| {
        return o;
    }
    // NaN against an int/uint: cel-go's compareDoubleInt treats NaN as
    // in-range and equal after the cast.
    if (a == .double_t and std.math.isNan(a.double_t)) return .eq;
    if (b == .double_t and std.math.isNan(b.double_t)) return .eq;
    return .eq;
}

fn minMaxPair(c: *Ctx, a: Value, b: Value, is_min: bool) Error!Value {
    if (!a.isNumeric() or !b.isNumeric()) {
        return c.noOverload(if (is_min) .math_min else .math_max, &.{ a, b });
    }
    const ord = extCompare(a, b);
    // cel-go minPair returns the first unless the first is greater.
    if (is_min) {
        return if (ord == .gt) b else a;
    }
    return if (ord == .lt) b else a;
}

fn minMax(c: *Ctx, args: []const Value, is_min: bool) Error!Value {
    const id: FnId = if (is_min) .math_min else .math_max;
    switch (args.len) {
        1 => {
            // Identity for scalars; fold for lists.
            if (args[0] == .list_t) {
                const l = args[0].list_t;
                if (l.len == 0) {
                    return c.fail("{s}(list) argument must not be empty", .{stdlib.fnName(id)});
                }
                var acc = l[0];
                for (l[1..]) |item| {
                    acc = try minMaxPair(c, acc, item, is_min);
                }
                if (!acc.isNumeric()) {
                    return c.fail("no such overload: {s}", .{stdlib.fnName(id)});
                }
                return acc;
            }
            return args[0];
        },
        2 => return minMaxPair(c, args[0], args[1], is_min),
        else => return c.noOverload(id, args),
    }
}

const RoundMode = enum { ceil, floor, round, trunc };

fn roundOp(c: *Ctx, args: []const Value, mode: RoundMode) Error!Value {
    const id: FnId = switch (mode) {
        .ceil => .math_ceil,
        .floor => .math_floor,
        .round => .math_round,
        .trunc => .math_trunc,
    };
    if (args.len != 1 or args[0] != .double_t) return c.noOverload(id, args);
    const d = args[0].double_t;
    if (std.math.isNan(d) or std.math.isInf(d)) return args[0];
    const r: f64 = switch (mode) {
        .ceil => @ceil(d),
        .floor => @floor(d),
        .round => @round(d), // half away from zero, like Go's math.Round
        .trunc => @trunc(d),
    };
    return .{ .double_t = r };
}

fn abs(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1) return c.noOverload(.math_abs, args);
    switch (args[0]) {
        .int_t => |i| {
            if (i == std.math.minInt(i64)) {
                return c.fail("return error for overflow: math.abs() of min int", .{});
            }
            return .{ .int_t = if (i < 0) -i else i };
        },
        .uint_t => return args[0],
        .double_t => |d| return .{ .double_t = @abs(d) },
        else => return c.noOverload(.math_abs, args),
    }
}

fn sign(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1) return c.noOverload(.math_sign, args);
    switch (args[0]) {
        .int_t => |i| return .{ .int_t = if (i > 0) 1 else if (i < 0) -1 else 0 },
        .uint_t => |u| return .{ .uint_t = if (u == 0) 0 else 1 },
        .double_t => |d| {
            if (std.math.isNan(d)) return args[0];
            if (d > 0) return .{ .double_t = 1 };
            if (d < 0) return .{ .double_t = -1 };
            return .{ .double_t = 0 };
        },
        else => return c.noOverload(.math_sign, args),
    }
}

fn isInfFn(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1 or args[0] != .double_t) return c.noOverload(.math_is_inf, args);
    return .{ .bool_t = std.math.isInf(args[0].double_t) };
}

fn isNanFn(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1 or args[0] != .double_t) return c.noOverload(.math_is_nan, args);
    return .{ .bool_t = std.math.isNan(args[0].double_t) };
}

fn isFiniteFn(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1 or args[0] != .double_t) return c.noOverload(.math_is_finite, args);
    const d = args[0].double_t;
    return .{ .bool_t = !std.math.isInf(d) and !std.math.isNan(d) };
}

fn sqrt(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1) return c.noOverload(.math_sqrt, args);
    const d: f64 = switch (args[0]) {
        .double_t => |d| d,
        .int_t => |i| @floatFromInt(i),
        .uint_t => |u| @floatFromInt(u),
        else => return c.noOverload(.math_sqrt, args),
    };
    return .{ .double_t = @sqrt(d) };
}

const BitOp = enum { bit_and, bit_or, bit_xor };

fn bitOp(c: *Ctx, args: []const Value, op: BitOp) Error!Value {
    const id: FnId = switch (op) {
        .bit_and => .math_bit_and,
        .bit_or => .math_bit_or,
        .bit_xor => .math_bit_xor,
    };
    if (args.len != 2) return c.noOverload(id, args);
    if (args[0] == .int_t and args[1] == .int_t) {
        const r: i64 = switch (op) {
            .bit_and => args[0].int_t & args[1].int_t,
            .bit_or => args[0].int_t | args[1].int_t,
            .bit_xor => args[0].int_t ^ args[1].int_t,
        };
        return .{ .int_t = r };
    }
    if (args[0] == .uint_t and args[1] == .uint_t) {
        const r: u64 = switch (op) {
            .bit_and => args[0].uint_t & args[1].uint_t,
            .bit_or => args[0].uint_t | args[1].uint_t,
            .bit_xor => args[0].uint_t ^ args[1].uint_t,
        };
        return .{ .uint_t = r };
    }
    return c.noOverload(id, args);
}

fn bitNot(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1) return c.noOverload(.math_bit_not, args);
    switch (args[0]) {
        .int_t => |i| return .{ .int_t = ~i },
        .uint_t => |u| return .{ .uint_t = ~u },
        else => return c.noOverload(.math_bit_not, args),
    }
}

fn bitShift(c: *Ctx, args: []const Value, left: bool) Error!Value {
    const id: FnId = if (left) .math_bit_shift_left else .math_bit_shift_right;
    if (args.len != 2) return c.noOverload(id, args);
    if (args[1] != .int_t) return c.noOverload(id, args);
    const bits = args[1].int_t;
    if (bits < 0) {
        return c.fail("math.{s}() negative offset: {d}", .{ if (left) "bitShiftLeft" else "bitShiftRight", bits });
    }
    // Go semantics: shifts >= bit width produce 0.
    if (bits >= 64) {
        if (args[0] == .int_t or args[0] == .uint_t) return .{ .int_t = 0 };
        return c.noOverload(id, args);
    }
    const n: u6 = @intCast(bits);
    if (left) {
        if (args[0] == .int_t) return .{ .int_t = args[0].int_t << n };
        if (args[0] == .uint_t) return .{ .uint_t = args[0].uint_t << n };
    } else {
        // Right shift of a signed int is LOGICAL (cast to uint), per cel-go.
        if (args[0] == .int_t) return .{ .int_t = @bitCast(@as(u64, @bitCast(args[0].int_t)) >> n) };
        if (args[0] == .uint_t) return .{ .uint_t = args[0].uint_t >> n };
    }
    return c.noOverload(id, args);
}

// ---- strings extension ----

/// Iterate the code points of a (valid UTF-8) string.
const CodePoints = struct {
    s: []const u8,
    i: usize = 0,

    fn next(cp: *CodePoints) ?u21 {
        if (cp.i >= cp.s.len) return null;
        const b = cp.s[cp.i];
        if (b < 0x80) {
            cp.i += 1;
            return b;
        }
        const l = std.unicode.utf8ByteSequenceLength(b) catch {
            cp.i += 1;
            return 0xFFFD;
        };
        const start = cp.i;
        cp.i += l;
        if (cp.i > cp.s.len) {
            cp.i = cp.s.len;
            return 0xFFFD;
        }
        return std.unicode.utf8Decode(cp.s[start..cp.i]) catch 0xFFFD;
    }
};

fn countCodePoints(s: []const u8) usize {
    // Each code point has exactly one non-continuation lead byte
    // (ASCII bytes count as leads). Valid UTF-8 only.
    var n: usize = 0;
    for (s) |b| {
        if (b & 0xC0 != 0x80) n += 1;
    }
    return n;
}

/// Byte length of the code point starting at `off`.
fn codePointLen(s: []const u8, off: usize) usize {
    const b = s[off];
    if (b < 0x80) return 1;
    return std.unicode.utf8ByteSequenceLength(b) catch 1;
}

/// Byte offset of code point index `idx`, or null when out of range.
fn byteOffsetOf(s: []const u8, idx: usize) ?usize {
    var i: usize = 0;
    var off: usize = 0;
    while (i < idx) : (i += 1) {
        if (off >= s.len) return null;
        off += codePointLen(s, off);
        if (off > s.len) return null;
    }
    return off;
}

fn argInt(args: []const Value, i: usize) ?i64 {
    if (i >= args.len) return null;
    return switch (args[i]) {
        .int_t => |v| v,
        else => null,
    };
}

fn argString(args: []const Value, i: usize) ?[]const u8 {
    if (i >= args.len) return null;
    return switch (args[i]) {
        .string_t => |v| v,
        else => null,
    };
}

fn charAt(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 2) return c.noOverload(.ext_char_at, args);
    const s = argString(args, 0) orelse return c.noOverload(.ext_char_at, args);
    const i = argInt(args, 1) orelse return c.noOverload(.ext_char_at, args);
    if (i < 0 or i > std.math.maxInt(usize)) {
        return c.fail("index out of range: {d}", .{i});
    }
    const n = countCodePoints(s);
    const idx: usize = @intCast(i);
    if (idx > n) return c.fail("index out of range: {d}", .{i});
    if (idx == n) return .{ .string_t = "" };
    const off = byteOffsetOf(s, idx).?;
    const l = std.unicode.utf8ByteSequenceLength(s[off]) catch 1;
    return .{ .string_t = s[off .. off + l] };
}

fn indexOf(c: *Ctx, args: []const Value, last: bool) Error!Value {
    const id: FnId = if (last) .ext_last_index_of else .ext_index_of;
    if (args.len < 2 or args.len > 3) return c.noOverload(id, args);
    const s = argString(args, 0) orelse return c.noOverload(id, args);
    const sub = argString(args, 1) orelse return c.noOverload(id, args);
    const n = countCodePoints(s);
    const sub_n = countCodePoints(sub);

    var offset: i64 = if (last) @as(i64, @intCast(n)) - 1 else 0;
    const has_offset = args.len == 3;
    if (has_offset) {
        offset = argInt(args, 2) orelse return c.noOverload(id, args);
    }

    if (sub.len == 0) {
        if (offset < 0) {
            return c.fail("index out of range: {d}", .{offset});
        }
        const off0: usize = @intCast(offset);
        if (!has_offset and last) {
            // cel-go: lastIndexOf of the empty string is the length.
            return .{ .int_t = @intCast(n) };
        }
        // Empty substring matches at the search offset, clamped to the end.
        if (off0 > n) return .{ .int_t = @intCast(n) };
        return .{ .int_t = offset };
    }
    // A string shorter than the substring cannot contain it (cel-go checks
    // byte lengths; this also covers the empty-string default offset of
    // -1 for lastIndexOf on an empty string).
    if (s.len < sub.len) {
        return .{ .int_t = -1 };
    }
    if (offset < 0) {
        return c.fail("index out of range: {d}", .{offset});
    }
    const off: usize = @intCast(offset);
    // An offset past the end of the string is an error (matching the
    // cel-spec testdata, which cel-go skips pending its own update).
    if (off > n) {
        return c.fail("index out of range: {d}", .{offset});
    }
    if (off >= n) {
        return .{ .int_t = -1 };
    }

    if (!last) {
        // Walk code point boundaries forward from `off`, comparing the
        // sub-string's bytes at each boundary (UTF-8 equality at a
        // boundary is code point equality).
        var boff = byteOffsetOf(s, off) orelse return .{ .int_t = -1 };
        var i: usize = off;
        while (i + sub_n <= n) : (i += 1) {
            if (boff + sub.len <= s.len and std.mem.eql(u8, s[boff .. boff + sub.len], sub)) {
                return .{ .int_t = @intCast(i) };
            }
            boff += codePointLen(s, boff);
        }
        return .{ .int_t = -1 };
    }

    // lastIndexOf: walk boundaries backward from the (clamped) offset.
    var boff = byteOffsetOf(s, off).?;
    var i: usize = off;
    // Clamp so that i + sub_n <= n.
    const max_start: usize = n - sub_n;
    if (i > max_start) {
        boff = byteOffsetOf(s, max_start).?;
        i = max_start;
    }
    while (true) {
        if (boff + sub.len <= s.len and std.mem.eql(u8, s[boff .. boff + sub.len], sub)) {
            return .{ .int_t = @intCast(i) };
        }
        if (i == 0) break;
        i -= 1;
        // Walk back over continuation bytes to the previous boundary.
        var b = boff - 1;
        while (b > 0 and (s[b] & 0xC0) == 0x80) b -= 1;
        boff = b;
    }
    return .{ .int_t = -1 };
}

fn asciiCase(c: *Ctx, args: []const Value, upper: bool) Error!Value {
    const id: FnId = if (upper) .ext_upper_ascii else .ext_lower_ascii;
    if (args.len != 1) return c.noOverload(id, args);
    const s = argString(args, 0) orelse return c.noOverload(id, args);
    const out = c.alloc.dupe(u8, s) catch return error.OutOfMemory;
    for (out) |*ch| {
        if (ch.* < 0x80) ch.* = if (upper) std.ascii.toUpper(ch.*) else std.ascii.toLower(ch.*);
    }
    return .{ .string_t = out };
}

fn replace(c: *Ctx, args: []const Value) Error!Value {
    if (args.len < 3 or args.len > 4) return c.noOverload(.ext_replace, args);
    const s = argString(args, 0) orelse return c.noOverload(.ext_replace, args);
    const old = argString(args, 1) orelse return c.noOverload(.ext_replace, args);
    const new = argString(args, 2) orelse return c.noOverload(.ext_replace, args);
    var limit: i64 = -1;
    if (args.len == 4) {
        limit = argInt(args, 3) orelse return c.noOverload(.ext_replace, args);
    }
    return replaceImpl(c, s, old, new, limit);
}

fn split(c: *Ctx, args: []const Value) Error!Value {
    if (args.len < 2 or args.len > 3) return c.noOverload(.ext_split, args);
    const s = argString(args, 0) orelse return c.noOverload(.ext_split, args);
    const sep = argString(args, 1) orelse return c.noOverload(.ext_split, args);
    var n: i64 = -1;
    if (args.len == 3) {
        n = argInt(args, 2) orelse return c.noOverload(.ext_split, args);
    }

    var parts: std.ArrayList(Value) = .empty;
    errdefer parts.deinit(c.alloc);

    if (n == 0) {
        return .{ .list_t = parts.toOwnedSlice(c.alloc) catch return error.OutOfMemory };
    }

    var rest = s;
    var count: i64 = 0;
    while (true) {
        if (n > 0 and count + 1 == n) break; // last part takes the remainder
        if (sep.len == 0) {
            // Go strings.Split with empty separator: one part per code point.
            break;
        }
        const idx = std.mem.indexOf(u8, rest, sep) orelse break;
        parts.append(c.alloc, .{ .string_t = rest[0..idx] }) catch return error.OutOfMemory;
        rest = rest[idx + sep.len ..];
        count += 1;
    }
    if (sep.len == 0 and !(n > 0)) {
        // Split into code points.
        var cp = CodePoints{ .s = rest };
        var off: usize = 0;
        while (cp.next()) |_| {
            parts.append(c.alloc, .{ .string_t = rest[off..cp.i] }) catch return error.OutOfMemory;
            off = cp.i;
        }
    } else if (sep.len == 0 and n > 0) {
        var cp = CodePoints{ .s = rest };
        var off: usize = 0;
        while (cp.next()) |_| {
            if (count + 1 == n) break;
            parts.append(c.alloc, .{ .string_t = rest[off..cp.i] }) catch return error.OutOfMemory;
            off = cp.i;
            count += 1;
        }
        parts.append(c.alloc, .{ .string_t = rest[off..] }) catch return error.OutOfMemory;
    } else {
        parts.append(c.alloc, .{ .string_t = rest }) catch return error.OutOfMemory;
    }
    return .{ .list_t = parts.toOwnedSlice(c.alloc) catch return error.OutOfMemory };
}

fn substring(c: *Ctx, args: []const Value) Error!Value {
    if (args.len < 2 or args.len > 3) return c.noOverload(.ext_substring, args);
    const s = argString(args, 0) orelse return c.noOverload(.ext_substring, args);
    const start = argInt(args, 1) orelse return c.noOverload(.ext_substring, args);
    const n = countCodePoints(s);
    if (args.len == 3) {
        const end = argInt(args, 2) orelse return c.noOverload(.ext_substring, args);
        if (start > end) {
            return c.fail("invalid substring range. start: {d}, end: {d}", .{ start, end });
        }
        if (start < 0 or start > n) {
            return c.fail("index out of range: {d}", .{start});
        }
        if (end < 0 or end > n) {
            return c.fail("index out of range: {d}", .{end});
        }
        const a = byteOffsetOf(s, @intCast(start)).?;
        const b = byteOffsetOf(s, @intCast(end)).?;
        return .{ .string_t = s[a..b] };
    }
    if (start < 0 or start > n) {
        return c.fail("index out of range: {d}", .{start});
    }
    const a = byteOffsetOf(s, @intCast(start)).?;
    return .{ .string_t = s[a..] };
}

fn isUnicodeSpace(cp: u21) bool {
    return switch (cp) {
        '\t', '\n', 0x0b, 0x0c, '\r', ' ', 0x85, 0xA0, 0x1680, 0x2000...0x200A, 0x2028, 0x2029, 0x202F, 0x205F, 0x3000 => true,
        else => false,
    };
}

fn trim(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1) return c.noOverload(.ext_trim, args);
    const s = argString(args, 0) orelse return c.noOverload(.ext_trim, args);
    var start: usize = 0;
    var end: usize = s.len;
    var cp = CodePoints{ .s = s };
    while (start < end) {
        // Decode the code point at `start`.
        const save = cp.i;
        cp.i = start;
        const next = cp.next() orelse break;
        if (!isUnicodeSpace(next)) break;
        start = cp.i;
        cp.i = save;
    }
    while (end > start) {
        // Find the last code point before `end`.
        var i = end - 1;
        while (i > start and (s[i] & 0xC0) == 0x80) i -= 1;
        var cp2 = CodePoints{ .s = s };
        cp2.i = i;
        const v = cp2.next() orelse break;
        if (!isUnicodeSpace(v)) break;
        end = i;
    }
    return .{ .string_t = s[start..end] };
}

fn join(c: *Ctx, args: []const Value) Error!Value {
    if (args.len < 1 or args.len > 2) return c.noOverload(.ext_join, args);
    if (args[0] != .list_t) return c.noOverload(.ext_join, args);
    const sep: []const u8 = if (args.len == 2)
        (argString(args, 1) orelse return c.noOverload(.ext_join, args))
    else
        "";
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(c.alloc);
    for (args[0].list_t, 0..) |item, i| {
        if (item != .string_t) {
            return c.fail("join: invalid input", .{});
        }
        if (i > 0) out.appendSlice(c.alloc, sep) catch return error.OutOfMemory;
        out.appendSlice(c.alloc, item.string_t) catch return error.OutOfMemory;
    }
    return .{ .string_t = out.toOwnedSlice(c.alloc) catch return error.OutOfMemory };
}

fn quote(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1 or args[0] != .string_t) return c.noOverload(.ext_strings_quote, args);
    const s = args[0].string_t;
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(c.alloc);
    out.append(c.alloc, '"') catch return error.OutOfMemory;
    var cp = CodePoints{ .s = s };
    while (cp.next()) |ch| {
        switch (ch) {
            0x07 => out.appendSlice(c.alloc, "\\a") catch return error.OutOfMemory,
            0x08 => out.appendSlice(c.alloc, "\\b") catch return error.OutOfMemory,
            0x0c => out.appendSlice(c.alloc, "\\f") catch return error.OutOfMemory,
            '\n' => out.appendSlice(c.alloc, "\\n") catch return error.OutOfMemory,
            '\r' => out.appendSlice(c.alloc, "\\r") catch return error.OutOfMemory,
            '\t' => out.appendSlice(c.alloc, "\\t") catch return error.OutOfMemory,
            0x0b => out.appendSlice(c.alloc, "\\v") catch return error.OutOfMemory,
            '\\' => out.appendSlice(c.alloc, "\\\\") catch return error.OutOfMemory,
            '"' => out.appendSlice(c.alloc, "\\\"") catch return error.OutOfMemory,
            else => {
                var buf: [4]u8 = undefined;
                const l = std.unicode.utf8Encode(ch, &buf) catch continue;
                out.appendSlice(c.alloc, buf[0..l]) catch return error.OutOfMemory;
            },
        }
    }
    out.append(c.alloc, '"') catch return error.OutOfMemory;
    return .{ .string_t = out.toOwnedSlice(c.alloc) catch return error.OutOfMemory };
}

fn reverse(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 1 or args[0] != .string_t) return c.noOverload(.ext_reverse, args);
    const s = args[0].string_t;
    // Reverse code points: collect each code point's start offset, then
    // emit the slices back-to-front.
    var starts: std.ArrayList(usize) = .empty;
    defer starts.deinit(c.alloc);
    {
        var i: usize = 0;
        while (i < s.len) {
            starts.append(c.alloc, i) catch return error.OutOfMemory;
            const l = std.unicode.utf8ByteSequenceLength(s[i]) catch 1;
            i += l;
        }
    }
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(c.alloc);
    var prev: usize = s.len;
    var i: usize = starts.items.len;
    while (i > 0) {
        i -= 1;
        const start = starts.items[i];
        out.appendSlice(c.alloc, s[start..prev]) catch return error.OutOfMemory;
        prev = start;
    }
    return .{ .string_t = out.toOwnedSlice(c.alloc) catch return error.OutOfMemory };
}

// ---- string.format (v2 semantics) ----

fn format(c: *Ctx, args: []const Value) Error!Value {
    if (args.len != 2) return c.noOverload(.ext_format, args);
    const fmt_str = argString(args, 0) orelse return c.noOverload(.ext_format, args);
    if (args[1] != .list_t) return c.noOverload(.ext_format, args);
    const fmt_args = args[1].list_t;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(c.alloc);
    var arg_index: usize = 0;
    var i: usize = 0;
    while (i < fmt_str.len) {
        if (fmt_str[i] != '%') {
            out.append(c.alloc, fmt_str[i]) catch return error.OutOfMemory;
            i += 1;
            continue;
        }
        if (i + 1 < fmt_str.len and fmt_str[i + 1] == '%') {
            out.append(c.alloc, '%') catch return error.OutOfMemory;
            i += 2;
            continue;
        }
        if (i + 1 >= fmt_str.len) {
            return c.fail("unexpected end of string", .{});
        }
        if (arg_index >= fmt_args.len) {
            return c.fail("index {d} out of range", .{arg_index});
        }
        const consumed = try formatClause(c, fmt_str[i + 1 ..], fmt_args[arg_index], &out);
        i += 1 + consumed;
        arg_index += 1;
    }
    return .{ .string_t = out.toOwnedSlice(c.alloc) catch return error.OutOfMemory };
}

/// Format one clause; returns the number of bytes consumed from `spec`
/// (after the '%').
fn formatClause(c: *Ctx, spec: []const u8, arg: Value, out: *std.ArrayList(u8)) Error!usize {
    var i: usize = 0;
    // Optional precision.
    var precision: ?usize = null;
    if (i < spec.len and spec[i] == '.') {
        i += 1;
        const digits_start = i;
        while (i < spec.len and spec[i] >= '0' and spec[i] <= '9') i += 1;
        if (i == digits_start) {
            return c.fail("could not parse formatting clause: could not find end of precision specifier", .{});
        }
        const p = std.fmt.parseInt(usize, spec[digits_start..i], 10) catch {
            return c.fail("could not parse formatting clause: invalid precision", .{});
        };
        if (p > 100) {
            return c.fail("could not parse formatting clause: precision {d} exceeds maximum allowed precision 100", .{p});
        }
        precision = p;
    }
    if (i >= spec.len) {
        return c.fail("could not parse formatting clause: unexpected end of string", .{});
    }
    const verb = spec[i];
    i += 1;
    switch (verb) {
        's' => try formatStringV2(c, arg, out),
        'd' => try formatDecimalV2(c, arg, out),
        'f' => try formatFixedV2(c, arg, precision orelse 6, out),
        'e' => try formatScientificV2(c, arg, precision orelse 6, out),
        'b' => try formatBinaryV2(c, arg, out),
        'x', 'X' => try formatHexV2(c, arg, verb == 'X', out),
        'o' => try formatOctalV2(c, arg, out),
        else => return c.fail("could not parse formatting clause: unrecognized formatting clause \"{c}\"", .{verb}),
    }
    return i;
}

/// The %s formatting: like the display form, but bytes with invalid UTF-8
/// are sanitized and maps are sorted by their formatted keys.
fn formatStringV2(c: *Ctx, v: Value, out: *std.ArrayList(u8)) Error!void {
    switch (v) {
        .null_t => out.appendSlice(c.alloc, "null") catch return error.OutOfMemory,
        .bool_t => |b| out.appendSlice(c.alloc, if (b) "true" else "false") catch return error.OutOfMemory,
        .int_t => |x| try appendInt(c.alloc, out, x),
        .uint_t => |x| try appendUint(c.alloc, out, x),
        .double_t => |d| {
            if (std.math.isNan(d)) {
                out.appendSlice(c.alloc, "NaN") catch return error.OutOfMemory;
            } else if (std.math.isInf(d)) {
                out.appendSlice(c.alloc, if (d > 0) "Infinity" else "-Infinity") catch return error.OutOfMemory;
            } else {
                appendShortestDouble(c.alloc, out, d) catch return error.OutOfMemory;
            }
        },
        .string_t => |s| out.appendSlice(c.alloc, s) catch return error.OutOfMemory,
        .bytes_t => |s| appendSanitizedUtf8(c.alloc, out, s) catch return error.OutOfMemory,
        .type_t => |t| out.appendSlice(c.alloc, t.name()) catch return error.OutOfMemory,
        .duration_t => |d| {
            var daw = std.Io.Writer.Allocating.init(c.alloc);
            time.formatDuration(d, &daw.writer) catch return error.OutOfMemory;
            out.appendSlice(c.alloc, daw.written()) catch return error.OutOfMemory;
        },
        .timestamp_t => |t| {
            const buf = time.formatTimestamp(t);
            out.appendSlice(c.alloc, std.mem.sliceTo(&buf, 0)) catch return error.OutOfMemory;
        },
        .list_t => |l| {
            out.append(c.alloc, '[') catch return error.OutOfMemory;
            for (l, 0..) |item, k| {
                if (k > 0) out.appendSlice(c.alloc, ", ") catch return error.OutOfMemory;
                try formatStringV2(c, item, out);
            }
            out.append(c.alloc, ']') catch return error.OutOfMemory;
        },
        .map_t => |m| {
            // Sort entries by their formatted key (stable).
            const Entry = struct { key: []const u8, value_str: []const u8 };
            var entries: std.ArrayList(Entry) = .empty;
            defer entries.deinit(c.alloc);
            for (m.entries) |e| {
                var kb: std.ArrayList(u8) = .empty;
                defer kb.deinit(c.alloc);
                try formatStringV2(c, e.key, &kb);
                var vb: std.ArrayList(u8) = .empty;
                defer vb.deinit(c.alloc);
                try formatStringV2(c, e.value, &vb);
                entries.append(c.alloc, .{
                    .key = kb.toOwnedSlice(c.alloc) catch return error.OutOfMemory,
                    .value_str = vb.toOwnedSlice(c.alloc) catch return error.OutOfMemory,
                }) catch return error.OutOfMemory;
            }
            std.mem.sort(Entry, entries.items, {}, struct {
                fn lt(_: void, a: Entry, b: Entry) bool {
                    return std.mem.lessThan(u8, a.key, b.key);
                }
            }.lt);
            out.append(c.alloc, '{') catch return error.OutOfMemory;
            for (entries.items, 0..) |e, k| {
                if (k > 0) out.appendSlice(c.alloc, ", ") catch return error.OutOfMemory;
                out.appendSlice(c.alloc, e.key) catch return error.OutOfMemory;
                out.appendSlice(c.alloc, ": ") catch return error.OutOfMemory;
                out.appendSlice(c.alloc, e.value_str) catch return error.OutOfMemory;
            }
            out.append(c.alloc, '}') catch return error.OutOfMemory;
        },
    }
}

/// Append `x` (treated as unsigned) in the given base.
fn appendIntBase(alloc: std.mem.Allocator, out: *std.ArrayList(u8), x: u64, base: u8, upper: bool) Error!void {
    var buf: [64]u8 = undefined;
    var i: usize = buf.len;
    var v = x;
    const digits = if (upper) "0123456789ABCDEF" else "0123456789abcdef";
    while (true) {
        i -= 1;
        buf[i] = digits[@intCast(v % base)];
        v /= base;
        if (v == 0) break;
    }
    out.appendSlice(alloc, buf[i..]) catch return error.OutOfMemory;
}

fn appendHexBytes(alloc: std.mem.Allocator, out: *std.ArrayList(u8), s: []const u8, upper: bool) Error!void {
    const digits = if (upper) "0123456789ABCDEF" else "0123456789abcdef";
    for (s) |b| {
        out.append(alloc, digits[b >> 4]) catch return error.OutOfMemory;
        out.append(alloc, digits[b & 0xF]) catch return error.OutOfMemory;
    }
}

fn appendInt(alloc: std.mem.Allocator, out: *std.ArrayList(u8), x: i64) Error!void {
    var buf: [24]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}", .{x}) catch unreachable;
    out.appendSlice(alloc, s) catch return error.OutOfMemory;
}

fn appendUint(alloc: std.mem.Allocator, out: *std.ArrayList(u8), x: u64) Error!void {
    var buf: [24]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}", .{x}) catch unreachable;
    out.appendSlice(alloc, s) catch return error.OutOfMemory;
}

fn appendShortestDouble(alloc: std.mem.Allocator, out: *std.ArrayList(u8), d: f64) !void {
    var aw = std.Io.Writer.Allocating.init(alloc);
    try aw.writer.printFloat(d, .{ .mode = .decimal });
    try out.appendSlice(alloc, aw.written());
}

/// Append bytes as UTF-8 with invalid sequences replaced by U+FFFD.
fn appendSanitizedUtf8(alloc: std.mem.Allocator, out: *std.ArrayList(u8), s: []const u8) !void {
    var i: usize = 0;
    while (i < s.len) {
        const l = std.unicode.utf8ByteSequenceLength(s[i]) catch {
            try out.appendSlice(alloc, "\xef\xbf\xbd");
            i += 1;
            continue;
        };
        if (i + l > s.len or !std.unicode.utf8ValidateSlice(s[i .. i + l])) {
            try out.appendSlice(alloc, "\xef\xbf\xbd");
            i += 1;
            continue;
        }
        try out.appendSlice(alloc, s[i .. i + l]);
        i += l;
    }
}

fn formatDecimalV2(c: *Ctx, v: Value, out: *std.ArrayList(u8)) Error!void {
    switch (v) {
        .int_t => |x| try appendInt(c.alloc, out, x),
        .uint_t => |x| try appendUint(c.alloc, out, x),
        .double_t => |d| {
            if (std.math.isNan(d)) {
                out.appendSlice(c.alloc, "NaN") catch return error.OutOfMemory;
            } else if (std.math.isInf(d)) {
                out.appendSlice(c.alloc, if (d > 0) "Infinity" else "-Infinity") catch return error.OutOfMemory;
            } else {
                return c.fail("error during formatting: decimal clause can only be used on integers, was given double", .{});
            }
        },
        else => return c.fail("error during formatting: decimal clause can only be used on integers, was given {s}", .{v.typeOf().name()}),
    }
}

fn formatFixedV2(c: *Ctx, v: Value, precision: usize, out: *std.ArrayList(u8)) Error!void {
    const d: f64 = switch (v) {
        .double_t => |d| d,
        .int_t => |x| @floatFromInt(x),
        .uint_t => |x| @floatFromInt(x),
        else => return c.fail("error during formatting: fixed-point clause can only be used on doubles, was given {s}", .{v.typeOf().name()}),
    };
    if (std.math.isNan(d)) {
        out.appendSlice(c.alloc, "NaN") catch return error.OutOfMemory;
        return;
    }
    if (std.math.isInf(d)) {
        out.appendSlice(c.alloc, if (d > 0) "Infinity" else "-Infinity") catch return error.OutOfMemory;
        return;
    }
    formatDoubleExact(c.alloc, out, d, precision, .fixed) catch return error.OutOfMemory;
}

fn formatScientificV2(c: *Ctx, v: Value, precision: usize, out: *std.ArrayList(u8)) Error!void {
    const d: f64 = switch (v) {
        .double_t => |d| d,
        .int_t => |x| @floatFromInt(x),
        .uint_t => |x| @floatFromInt(x),
        else => return c.fail("error during formatting: scientific clause can only be used on doubles, was given {s}", .{v.typeOf().name()}),
    };
    if (std.math.isNan(d)) {
        out.appendSlice(c.alloc, "NaN") catch return error.OutOfMemory;
        return;
    }
    if (std.math.isInf(d)) {
        out.appendSlice(c.alloc, if (d > 0) "Infinity" else "-Infinity") catch return error.OutOfMemory;
        return;
    }
    formatDoubleExact(c.alloc, out, d, precision, .scientific) catch return error.OutOfMemory;
}

fn formatBinaryV2(c: *Ctx, v: Value, out: *std.ArrayList(u8)) Error!void {
    switch (v) {
        .int_t => |x| try appendIntBase(c.alloc, out, @bitCast(@as(u64, @bitCast(x))), 2, false),
        .uint_t => |x| try appendIntBase(c.alloc, out, x, 2, false),
        .bool_t => |b| out.appendSlice(c.alloc, if (b) "1" else "0") catch return error.OutOfMemory,
        else => return c.fail("error during formatting: only integers and bools can be formatted as binary, was given {s}", .{v.typeOf().name()}),
    }
}

fn formatHexV2(c: *Ctx, v: Value, upper: bool, out: *std.ArrayList(u8)) Error!void {
    switch (v) {
        .int_t => |x| try appendIntBase(c.alloc, out, @bitCast(@as(u64, @bitCast(x))), 16, upper),
        .uint_t => |x| try appendIntBase(c.alloc, out, x, 16, upper),
        .string_t => |s| try appendHexBytes(c.alloc, out, s, upper),
        .bytes_t => |s| try appendHexBytes(c.alloc, out, s, upper),
        else => return c.fail("error during formatting: only integers, byte buffers, and strings can be formatted as hex, was given {s}", .{v.typeOf().name()}),
    }
}

fn formatOctalV2(c: *Ctx, v: Value, out: *std.ArrayList(u8)) Error!void {
    switch (v) {
        .int_t => |x| try appendIntBase(c.alloc, out, @bitCast(@as(u64, @bitCast(x))), 8, false),
        .uint_t => |x| try appendIntBase(c.alloc, out, x, 8, false),
        else => return c.fail("error during formatting: octal clause can only be used on integers, was given {s}", .{v.typeOf().name()}),
    }
}

// ---- exact-decimal double formatting (Go strconv-compatible) ----

/// A finite double's exact value is m * 2^e with a 53-bit m. Convert to an
/// exact decimal digit string plus decimal-point position, then round
/// half-to-even at the requested precision (matching Go's strconv, which
/// the cel-go format clauses use).
const ExactDecimal = struct {
    /// Digits, most-significant first, no leading zeros ("0" for zero).
    digits: [800]u8 = undefined,
    n_digits: usize = 0,
    /// Number of digits before the decimal point (may be negative for
    /// small fractions).
    point: i32 = 0,
    neg: bool = false,

    fn fromDouble(d: f64) ExactDecimal {
        var ed = ExactDecimal{};
        if (d < 0 or (d == 0 and std.math.signbit(d))) ed.neg = true;
        const bits: u64 = @bitCast(d);
        const biased: i32 = @intCast((bits >> 52) & 0x7FF);
        const mant = bits & ((1 << 52) - 1);
        var m: u64 = undefined;
        var e: i32 = undefined;
        if (biased == 0) {
            m = mant;
            e = -1074;
        } else {
            m = mant | (1 << 52);
            e = biased - 1075;
        }
        if (m == 0) {
            ed.digits[0] = '0';
            ed.n_digits = 1;
            ed.point = 1;
            return ed;
        }

        // Little-endian base-2^32 limbs. Max width:
        //   e >= 0: 53 + e <= 1024 bits
        //   e < 0:  53 + bits(5^1074) <= ~2550 bits
        var limbs: [100]u32 = @splat(0);
        var n_limbs: usize = 1;
        limbs[0] = @truncate(m);
        if (m >> 32 != 0) {
            limbs[1] = @truncate(m >> 32);
            n_limbs = 2;
        }

        if (e >= 0) {
            // value = m << e (an integer).
            const limb_shift: usize = @intCast(@divTrunc(e, 32));
            const bit_shift: u5 = @intCast(@mod(e, 32));
            var shifted: [100]u32 = @splat(0);
            var carry: u32 = 0;
            var i: usize = 0;
            while (i < n_limbs) : (i += 1) {
                const lo: u64 = @as(u64, limbs[i]) << bit_shift;
                shifted[limb_shift + i] = @as(u32, @truncate(lo)) | carry;
                carry = @as(u32, @truncate(lo >> 32));
            }
            var total = limb_shift + n_limbs;
            if (carry != 0) {
                shifted[total] = carry;
                total += 1;
            }
            limbs = shifted;
            n_limbs = total;
        } else {
            // value = m * 5^k / 10^k with k = -e: multiply m by 5^k, then
            // place the decimal point k digits from the right.
            const k: u32 = @intCast(-e);
            var remaining = k;
            // 5^13 = 1220703125: multiply in chunks of 13 fives.
            const chunk_pows: u32 = 13;
            while (remaining > 0) {
                const take: u32 = @min(remaining, chunk_pows);
                const factor: u64 = std.math.pow(u64, 5, take);
                remaining -= take;
                var carry: u64 = 0;
                var i: usize = 0;
                while (i < n_limbs or carry != 0) : (i += 1) {
                    const prod: u64 = if (i < n_limbs) @as(u64, limbs[i]) * factor + carry else carry;
                    limbs[i] = @truncate(prod);
                    carry = prod >> 32;
                }
                if (i > n_limbs) n_limbs = i;
            }
            ed.point = -@as(i32, @intCast(k));
        }

        // Convert limbs to decimal digits (least significant first), then
        // reverse.
        ed.n_digits = 0;
        var work = limbs;
        var wn = n_limbs;
        while (wn > 0) {
            var rem: u64 = 0;
            var i: usize = wn;
            while (i > 0) {
                i -= 1;
                const cur = (rem << 32) | work[i];
                work[i] = @truncate(cur / 1_000_000_000);
                rem = cur % 1_000_000_000;
            }
            while (wn > 0 and work[wn - 1] == 0) wn -= 1;
            var chunk_val: u32 = @intCast(rem);
            var digit_count: usize = 0;
            while (digit_count < 9) : (digit_count += 1) {
                ed.digits[ed.n_digits] = @intCast('0' + chunk_val % 10);
                ed.n_digits += 1;
                chunk_val /= 10;
            }
        }
        std.mem.reverse(u8, ed.digits[0..ed.n_digits]);
        // Strip leading zeros (keep one digit).
        var start_idx: usize = 0;
        while (start_idx + 1 < ed.n_digits and ed.digits[start_idx] == '0') start_idx += 1;
        if (start_idx > 0) {
            std.mem.copyForwards(u8, ed.digits[0 .. ed.n_digits - start_idx], ed.digits[start_idx..ed.n_digits]);
            ed.n_digits -= start_idx;
        }
        if (e >= 0) {
            ed.point = @intCast(ed.n_digits);
        } else {
            // digits represent m * 5^k; the value is digits * 10^-k, so the
            // decimal point sits k digits from the right.
            ed.point = @as(i32, @intCast(ed.n_digits)) - @as(i32, @intCast(-e));
        }
        return ed;
    }
};

const ExactMode = enum { fixed, scientific };

/// Format a finite double exactly like Go's strconv.FormatFloat(v, 'f'/'e',
/// precision, 64): round-half-even on the exact decimal expansion.
fn formatDoubleExact(alloc: std.mem.Allocator, out: *std.ArrayList(u8), d: f64, precision: usize, mode: ExactMode) !void {
    var ed = ExactDecimal.fromDouble(d);

    if (mode == .fixed) {
        const cut_i: i64 = @as(i64, @intCast(precision)) + ed.point;
        if (cut_i >= 0 and cut_i < @as(i64, @intCast(ed.n_digits))) {
            roundDigits(&ed, @intCast(cut_i));
        }
        // cut_i < 0: every digit is dropped and the half-even boundary
        // digit is a leading zero (even), so the result is zero-filled.
        if (ed.neg) try out.append(alloc, '-');
        const point: usize = if (ed.point <= 0) 0 else @intCast(ed.point);
        if (point == 0) {
            try out.append(alloc, '0');
        } else {
            var i: usize = 0;
            while (i < point) : (i += 1) {
                try out.append(alloc, digitAt(&ed, i));
            }
        }
        if (precision > 0) {
            try out.append(alloc, '.');
            var j: usize = 0;
            while (j < precision) : (j += 1) {
                try out.append(alloc, digitAt(&ed, point + j));
            }
        }
        return;
    }

    // Scientific: keep 1 + precision significant digits.
    const keep: usize = 1 + precision;
    var nz = firstNonzero(&ed);
    if (ed.n_digits - nz > keep) {
        roundDigits(&ed, nz + keep);
        nz = firstNonzero(&ed);
    }
    const exp10: i32 = ed.point - @as(i32, @intCast(nz)) - 1;
    if (ed.neg) try out.append(alloc, '-');
    try out.append(alloc, if (ed.n_digits > 0) ed.digits[nz] else '0');
    if (precision > 0) {
        try out.append(alloc, '.');
        var j: usize = 1;
        while (j <= precision) : (j += 1) {
            const idx = nz + j;
            try out.append(alloc, if (idx < ed.n_digits) ed.digits[idx] else '0');
        }
    }
    try out.append(alloc, 'e');
    try out.append(alloc, if (exp10 < 0) '-' else '+');
    const abs_exp: u32 = @abs(exp10);
    if (abs_exp < 10) {
        try out.append(alloc, '0');
    }
    var buf: [16]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}", .{abs_exp}) catch unreachable;
    try out.appendSlice(alloc, s);
}

fn firstNonzero(ed: *const ExactDecimal) usize {
    var i: usize = 0;
    while (i + 1 < ed.n_digits and ed.digits[i] == '0') i += 1;
    return i;
}

fn digitAt(ed: *const ExactDecimal, idx: usize) u8 {
    if (idx < ed.n_digits) return ed.digits[idx];
    return '0';
}

/// Round the digit string at index `cut` (digits before cut are kept),
/// half-to-even, with carry into earlier digits.
fn roundDigits(ed: *ExactDecimal, cut: usize) void {
    if (cut >= ed.n_digits) return;
    const dropped = ed.digits[cut..ed.n_digits];
    ed.n_digits = cut;
    const first = dropped[0];
    var round_up = false;
    if (first > '5') {
        round_up = true;
    } else if (first == '5') {
        var rest_nonzero = false;
        for (dropped[1..]) |ch| {
            if (ch != '0') {
                rest_nonzero = true;
                break;
            }
        }
        if (rest_nonzero) {
            round_up = true;
        } else {
            // Exactly half: round to even.
            const last_kept: u8 = if (cut > 0) ed.digits[cut - 1] else '0';
            round_up = (last_kept - '0') % 2 == 1;
        }
    }
    if (!round_up) {
        normalize(ed);
        return;
    }
    // Carry propagate.
    var i = cut;
    while (i > 0) {
        i -= 1;
        if (ed.digits[i] == '9') {
            ed.digits[i] = '0';
        } else {
            ed.digits[i] += 1;
            normalize(ed);
            return;
        }
    }
    // All nines carried: prepend a 1.
    std.mem.copyBackwards(u8, ed.digits[1 .. ed.n_digits + 1], ed.digits[0..ed.n_digits]);
    ed.digits[0] = '1';
    ed.n_digits += 1;
    ed.point += 1;
    normalize(ed);
}

fn normalize(ed: *ExactDecimal) void {
    var start: usize = 0;
    while (start + 1 < ed.n_digits and ed.digits[start] == '0') start += 1;
    if (start > 0) {
        std.mem.copyForwards(u8, ed.digits[0 .. ed.n_digits - start], ed.digits[start..ed.n_digits]);
        ed.n_digits -= start;
    }
}

/// Go strings.Replace semantics for the strings extension replace().
fn replaceImpl(c: *Ctx, s: []const u8, old: []const u8, new: []const u8, limit: i64) Error!Value {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(c.alloc);

    if (old.len == 0) {
        // Insert new before each code point and at the end, up to limit
        // replacements (n < 0 means all).
        var count: i64 = 0;
        var cp = CodePoints{ .s = s };
        var prev: usize = 0;
        while (cp.next()) |_| {
            if (limit >= 0 and count >= limit) break;
            out.appendSlice(c.alloc, new) catch return error.OutOfMemory;
            out.appendSlice(c.alloc, s[prev..cp.i]) catch return error.OutOfMemory;
            prev = cp.i;
            count += 1;
        }
        if (limit < 0 or count < limit) {
            out.appendSlice(c.alloc, new) catch return error.OutOfMemory;
        }
        out.appendSlice(c.alloc, s[prev..]) catch return error.OutOfMemory;
        return .{ .string_t = out.toOwnedSlice(c.alloc) catch return error.OutOfMemory };
    }

    var rest = s;
    var count: i64 = 0;
    while (limit < 0 or count < limit) {
        const idx = std.mem.indexOf(u8, rest, old) orelse break;
        out.appendSlice(c.alloc, rest[0..idx]) catch return error.OutOfMemory;
        out.appendSlice(c.alloc, new) catch return error.OutOfMemory;
        rest = rest[idx + old.len ..];
        count += 1;
    }
    out.appendSlice(c.alloc, rest) catch return error.OutOfMemory;
    return .{ .string_t = out.toOwnedSlice(c.alloc) catch return error.OutOfMemory };
}

test {
    _ = tests;
}

const tests = struct {
    const testing = std.testing;
    const parser = @import("parser.zig");
    const vm = @import("vm.zig");

    /// Evaluate via the full pipeline (parser macros + VM) with the given
    /// extensions enabled; returns the display form or null on error.
    fn evalExt(src_expr: []const u8, ext: Extensions) !?[]const u8 {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const a = arena.allocator();
        const r = try parser.parseSrc(a, src_expr, .{ .ext = ext });
        const tree = switch (r) {
            .ok => |t| t,
            .err => {
                std.debug.print("parse error: {s} (expr: {s})\n", .{ r.err.msg, src_expr });
                return null;
            },
        };
        const prog = try vm.Compiler.compile(a, tree, .{ .ext = ext });
        const result = try prog.run(a, &.{}, "", .{});
        const v = switch (result) {
            .value => |v| v,
            .err => |e| {
                std.debug.print("eval error: {s} (expr: {s})\n", .{ e, src_expr });
                return null;
            },
        };
        var aw = std.Io.Writer.Allocating.init(a);
        try v.format(&aw.writer);
        return try testing.allocator.dupe(u8, aw.written());
    }

    fn check(expr: []const u8, want: []const u8) !void {
        const got = (try evalExt(expr, .all)) orelse return error.TestUnexpectedResult;
        defer testing.allocator.free(got);
        testing.expectEqualStrings(want, got) catch |e| {
            std.debug.print("CHECK FAILED expr: {s}\n", .{expr});
            return e;
        };
    }

    fn checkErr(expr: []const u8) !void {
        const got = try evalExt(expr, .all);
        if (got) |g| {
            std.debug.print("expected error, got {s} for {s}\n", .{ g, expr });
            testing.allocator.free(g);
            return error.TestUnexpectedResult;
        }
    }

    test "math extension" {
        try check("math.greatest(1, 2)", "2");
        try check("math.least(1, 2)", "1");
        try check("math.greatest(3)", "3");
        try check("math.least([3, 1, 2])", "1");
        try check("math.greatest([3, 1, 2])", "3");
        try check("math.greatest(1, 1.0) == 1", "true");
        try check("math.greatest(5.4, 10, 3u, -5.0, 9223372036854775807) == 9223372036854775807", "true");
        try check("math.least(5.4, 10, 3u, -5.0, 3.5) == -5.0", "true");
        try check("math.greatest(1, 2, 3) == 3", "true");
        try checkErr("math.least()");
        try checkErr("math.least('a')");
        try checkErr("math.least(1, 'a')");
        try checkErr("math.least([])");
        try check("math.ceil(-1.2)", "-1");
        try check("math.ceil(1.2)", "2");
        try check("math.floor(-1.2)", "-2");
        try check("math.round(-1.5)", "-2");
        try check("math.round(1.5)", "2");
        try check("math.trunc(-1.2)", "-1");
        try check("math.abs(-11)", "11");
        try check("math.abs(1u)", "1");
        try check("math.abs(1.5)", "1.5");
        try checkErr("math.abs(-9223372036854775808)");
        try check("math.sign(100)", "1");
        try check("math.sign(-11)", "-1");
        try check("math.sign(0)", "0");
        try check("math.sign(100u)", "1");
        try check("math.sign(0u)", "0");
        try check("math.sign(-32.0)", "-1");
        try check("math.isNaN(0.0/0.0)", "true");
        try check("math.isInf(1.0/0.0)", "true");
        try check("math.isFinite(1.0/1.5)", "true");
        try checkErr("math.isNaN(dyn(true))");
        try check("math.bitAnd(1, 3)", "1");
        try check("math.bitOr(1, 2)", "3");
        try check("math.bitXor(1, 3)", "2");
        try check("math.bitNot(0)", "-1");
        try check("math.bitShiftLeft(1, 2)", "4");
        try check("math.bitShiftLeft(1, 200)", "0");
        try check("math.bitShiftRight(1024, 2)", "256");
        try check("math.bitShiftRight(-1024, 3)", "2305843009213693824");
        try check("math.bitShiftRight(1024, 64)", "0");
        try checkErr("math.bitShiftLeft(1u, -1)");
        try check("math.sqrt(4)", "2");
        try check("math.sqrt(2u)", "1.4142135623730951");
    }

    test "math extension is opt-in" {
        const got = try evalExt("math.greatest(1, 2)", .{});
        try testing.expect(got == null);
    }

    test "strings extension" {
        try check("'tacocat'.charAt(3)", "o");
        try check("'tacocat'.charAt(7)", "");
        try check("'©αT'.charAt(1) == 'α'", "true");
        try checkErr("'tacocat'.charAt(30)");
        try check("'tacocat'.indexOf('ac')", "1");
        try check("'tacocat'.indexOf('a', 3)", "5");
        try check("'ta©o©αT'.indexOf('©', 3)", "4");
        try check("'tacocat'.indexOf('none') == -1", "true");
        try check("'tacocat'.lastIndexOf('at')", "5");
        try check("'tacocat'.lastIndexOf('a', 3)", "1");
        try check("'ta©o©αT'.lastIndexOf('©', 3)", "2");
        try check("'tacocat'.lastIndexOf('')", "7");
        try checkErr("'tacocat'.lastIndexOf('a', -1)");
        try check("'TacoCat'.lowerAscii() == 'tacocat'", "true");
        try check("'TacoCÆt Xii'.lowerAscii() == 'tacocÆt xii'", "true");
        try check("'tacoCÆt Xii'.upperAscii() == 'TACOCÆT XII'", "true");
        try check("'{0} days {0} hours'.replace('{0}', '2') == '2 days 2 hours'", "true");
        try check("'{0} days {0} hours'.replace('{0}', '2', 1).replace('{0}', '23') == '2 days 23 hours'", "true");
        try check("'hello world'.split(' ') == ['hello', 'world']", "true");
        try check("'hello world events!'.split(' ', 0) == []", "true");
        try check("'hello world events!'.split(' ', 1) == ['hello world events!']", "true");
        try check("'tacocat'.substring(4) == 'cat'", "true");
        try check("'ta©o©αT'.substring(2, 6) == '©o©α'", "true");
        try checkErr("'tacocat'.substring(4, 3)");
        try checkErr("'tacocat'.substring(50)");
        try check("'  text  '.trim() == 'text'", "true");
        try check("'\\u180etext\\u200b'.trim() == '\\u180etext\\u200b'", "true");
        try check("['x', 'y'].join('-') == 'x-y'", "true");
        try check("[].join() == ''", "true");
        try checkErr("[1].join()");
        try check("'Ta©oCαt'.reverse() == 'tαCo©aT'", "true");
        try check("strings.quote('a\\nb') == '\"a\\\\nb\"'", "true");
    }

    test "strings format" {
        try check("\"str is %s and some more\".format([\"filler\"])", "str is filler and some more");
        try check("\"%% and also %%\".format([])", "% and also %");
        try check("\"%%%s%%\".format([\"text\"])", "%text%");
        try check("\"%d %d %d, %s %s %s\".format([1, 2, 3, \"A\", \"B\", \"C\"])", "1 2 3, A B C");
        try check("\"%.3f\".format([1.2345])", "1.234");
        try check("\"%f\".format([2.71828])", "2.718280");
        try check("\"%e\".format([2.71828])", "2.718280e+00");
        try check("\"%.6e\".format([1052.032911275])", "1.052033e+03");
        try check("\"%b\".format([5])", "101");
        try check("\"%b\".format([true])", "1");
        try check("\"%o\".format([11])", "13");
        try check("\"%x\".format([30])", "1e");
        try check("\"%X\".format([30])", "1E");
        try check("\"%x\".format([\"Hello world!\"])", "48656c6c6f20776f726c6421");
        try check("\"%x\".format([b\"byte string\"])", "6279746520737472696e67");
        try check("\"%s\".format([null])", "null");
        try check("\"%s\".format([[\"abc\", 3.14, null, [9, 8]]])", "[abc, 3.14, null, [9, 8]]");
        try check("\"%s\".format([{1: \"v1\", true: 2, \"a\": null}])", "{1: v1, a: null, true: 2}");
        try check("\"%s\".format([duration(\"2h\")])", "7200s");
        try check("\"%s\".format([timestamp(\"2023-02-03T23:31:20Z\")])", "2023-02-03T23:31:20Z");
        try checkErr("\"%a\".format([1])");
        try checkErr("\"%d %d %d\".format([0, 1])");
        try checkErr("\"%d\".format([null])");
        try checkErr("\"%e\".format([null])");
        try checkErr("\"%x\".format([0.5])");
        try checkErr("\"%b\".format([\"abc\"])");
        try checkErr("\"%o\".format([\"a string\"])");
    }

    test "exact decimal formatting" {
        const alloc = std.testing.allocator;
        const Case = struct { v: f64, prec: usize, mode: ExactMode, want: []const u8 };
        const cases = [_]Case{
            .{ .v = 2.5, .prec = 0, .mode = .fixed, .want = "2" }, // half-to-even
            .{ .v = 1.5, .prec = 0, .mode = .fixed, .want = "2" },
            .{ .v = 3.5, .prec = 0, .mode = .fixed, .want = "4" },
            .{ .v = 2.71828, .prec = 6, .mode = .fixed, .want = "2.718280" },
            .{ .v = 123.4999, .prec = 3, .mode = .fixed, .want = "123.500" },
            .{ .v = 123.4994, .prec = 3, .mode = .fixed, .want = "123.499" },
            .{ .v = -123.4999, .prec = 3, .mode = .fixed, .want = "-123.500" },
            .{ .v = -1.2, .prec = 5, .mode = .fixed, .want = "-1.20000" },
            .{ .v = 123.000000, .prec = 0, .mode = .fixed, .want = "123" },
            .{ .v = 1.2345, .prec = 3, .mode = .fixed, .want = "1.234" },
            .{ .v = 2, .prec = 6, .mode = .fixed, .want = "2.000000" },
            .{ .v = 1052.032911275, .prec = 6, .mode = .scientific, .want = "1.052033e+03" },
            .{ .v = 2.71828, .prec = 6, .mode = .scientific, .want = "2.718280e+00" },
            .{ .v = 2, .prec = 6, .mode = .scientific, .want = "2.000000e+00" },
            .{ .v = 0.000123, .prec = 3, .mode = .fixed, .want = "0.000" },
            .{ .v = 9.99, .prec = 1, .mode = .scientific, .want = "1.0e+01" },
            .{ .v = 1e300, .prec = 2, .mode = .scientific, .want = "1.00e+300" },
            .{ .v = 123456789.0, .prec = 0, .mode = .fixed, .want = "123456789" },
        };
        for (cases) |case| {
            var out: std.ArrayList(u8) = .empty;
            defer out.deinit(alloc);
            try formatDoubleExact(alloc, &out, case.v, case.prec, case.mode);
            try testing.expectEqualStrings(case.want, out.items);
        }
    }
};
