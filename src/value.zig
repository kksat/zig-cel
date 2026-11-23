//! CEL runtime values and their core semantics: heterogeneous equality
//! (numeric values compare on a shared number line), cross-type numeric
//! ordering, and ordered maps with an open-addressing index whose hash is
//! consistent with heterogeneous key equality.
//!
//! Values are immutable and allocated in an evaluation arena. The
//! representation is a 16-byte payload plus a 1-byte tag.

const std = @import("std");

/// Runtime types. `type_t` values carry one of these.
pub const Type = enum(u8) {
    null_type,
    bool_,
    int_,
    uint_,
    double_,
    string_,
    bytes_,
    list_,
    map_,
    type_,
    duration,
    timestamp,

    pub fn name(t: Type) []const u8 {
        return switch (t) {
            .null_type => "null_type",
            .bool_ => "bool",
            .int_ => "int",
            .uint_ => "uint",
            .double_ => "double",
            .string_ => "string",
            .bytes_ => "bytes",
            .list_ => "list",
            .map_ => "map",
            .type_ => "type",
            // Well-known protobuf-backed types keep their proto names.
            .duration => "google.protobuf.Duration",
            .timestamp => "google.protobuf.Timestamp",
        };
    }

    pub fn fromName(s: []const u8) ?Type {
        const map = .{
            .{ "null_type", Type.null_type },
            .{ "bool", Type.bool_ },
            .{ "int", Type.int_ },
            .{ "uint", Type.uint_ },
            .{ "double", Type.double_ },
            .{ "string", Type.string_ },
            .{ "bytes", Type.bytes_ },
            .{ "list", Type.list_ },
            .{ "map", Type.map_ },
            .{ "type", Type.type_ },
            .{ "google.protobuf.Duration", Type.duration },
            .{ "google.protobuf.Timestamp", Type.timestamp },
        };
        inline for (map) |e| {
            if (std.mem.eql(u8, e[0], s)) return e[1];
        }
        return null;
    }
};

/// A duration with second/nanosecond parts (google.protobuf.Duration range).
pub const Duration = struct {
    seconds: i64,
    nanos: i32,

    pub const min_seconds: i64 = -315576000000;
    pub const max_seconds: i64 = 315576000000;
};

/// A timestamp: seconds since the Unix epoch plus [0, 1e9) nanoseconds.
pub const Timestamp = struct {
    seconds: i64,
    nanos: u32,

    pub const min_seconds: i64 = -62135596800; // 0001-01-01T00:00:00Z
    pub const max_seconds: i64 = 253402300799; // 9999-12-31T23:59:59Z
};

pub const Value = union(enum) {
    null_t: void,
    bool_t: bool,
    int_t: i64,
    uint_t: u64,
    double_t: f64,
    string_t: []const u8,
    bytes_t: []const u8,
    list_t: []const Value,
    map_t: *const Map,
    type_t: Type,
    duration_t: Duration,
    timestamp_t: Timestamp,

    pub fn isNumeric(v: Value) bool {
        return switch (v) {
            .int_t, .uint_t, .double_t => true,
            else => false,
        };
    }

    /// Name of the runtime type of this value (for `type()` and errors).
    pub fn typeOf(v: Value) Type {
        return switch (v) {
            .null_t => .null_type,
            .bool_t => .bool_,
            .int_t => .int_,
            .uint_t => .uint_,
            .double_t => .double_,
            .string_t => .string_,
            .bytes_t => .bytes_,
            .list_t => .list_,
            .map_t => .map_,
            .type_t => .type_,
            .duration_t => .duration,
            .timestamp_t => .timestamp,
        };
    }

    /// Display form used by the CLI and tests.
    pub fn format(v: Value, w: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (v) {
            .null_t => try w.writeAll("null"),
            .bool_t => |b| try w.writeAll(if (b) "true" else "false"),
            .int_t => |i| try w.printInt(i, 10, .lower, .{}),
            .uint_t => |u| try w.printInt(u, 10, .lower, .{}),
            .double_t => |d| try w.printFloat(d, .{ .mode = .decimal }),
            .string_t => |s| try w.writeAll(s),
            .bytes_t => |b| {
                try w.writeAll("b\"");
                for (b) |c| {
                    if (c >= 0x20 and c < 0x7f and c != '"' and c != '\\') {
                        try w.writeByte(c);
                    } else {
                        try w.print("\\x{x:0>2}", .{c});
                    }
                }
                try w.writeAll("\"");
            },
            .list_t => |l| {
                try w.writeAll("[");
                for (l, 0..) |e, i| {
                    if (i > 0) try w.writeAll(", ");
                    try e.format(w);
                }
                try w.writeAll("]");
            },
            .map_t => |m| {
                try w.writeAll("{");
                for (m.entries, 0..) |e, i| {
                    if (i > 0) try w.writeAll(", ");
                    try e.key.format(w);
                    try w.writeAll(": ");
                    try e.value.format(w);
                }
                try w.writeAll("}");
            },
            .type_t => |t| try w.writeAll(t.name()),
            .duration_t => |d| try @import("time.zig").formatDuration(d, w),
            .timestamp_t => |t| {
                const buf = @import("time.zig").formatTimestamp(t);
                try w.writeAll("timestamp(");
                try w.writeAll(std.mem.sliceTo(&buf, 0));
                try w.writeAll(")");
            },
        }
    }

    // ---- equality ----

    /// Heterogeneous equality per the spec: numeric values of different
    /// representations compare on a shared number line (NaN != NaN);
    /// otherwise values must share a type. Lists compare elementwise, maps
    /// compare key sets and values.
    pub fn eql(a: Value, b: Value) bool {
        if (a.isNumeric() and b.isNumeric()) return numericEql(a, b);
        if (@intFromEnum(std.meta.activeTag(a)) != @intFromEnum(std.meta.activeTag(b))) return false;
        switch (a) {
            .null_t => return true,
            .bool_t => |x| return x == b.bool_t,
            .string_t => |x| return std.mem.eql(u8, x, b.string_t),
            .bytes_t => |x| return std.mem.eql(u8, x, b.bytes_t),
            .list_t => |x| {
                const y = b.list_t;
                if (x.len != y.len) return false;
                for (x, y) |xe, ye| {
                    if (!eql(xe, ye)) return false;
                }
                return true;
            },
            .map_t => |x| {
                const y = b.map_t;
                if (x.entries.len != y.entries.len) return false;
                for (x.entries) |e| {
                    const other = y.get(e.key) orelse return false;
                    if (!eql(e.value, other)) return false;
                }
                return true;
            },
            .type_t => |x| return x == b.type_t,
            .duration_t => |x| {
                const y = b.duration_t;
                return x.seconds == y.seconds and x.nanos == y.nanos;
            },
            .timestamp_t => |x| {
                const y = b.timestamp_t;
                return x.seconds == y.seconds and x.nanos == y.nanos;
            },
            else => return false,
        }
    }

    /// Exact equality between an i64 and an f64 on the number line.
    fn intDoubleEql(i: i64, d: f64) bool {
        if (std.math.isNan(d) or std.math.isInf(d)) return false;
        const t = @trunc(d);
        if (t != d) return false;
        if (t >= 9223372036854775808.0) return false; // >= 2^63
        if (t < -9223372036854775808.0) return false; // < -2^63
        return @as(i64, @intFromFloat(t)) == i;
    }

    /// Exact equality between a u64 and an f64 on the number line.
    fn uintDoubleEql(u: u64, d: f64) bool {
        if (std.math.isNan(d) or std.math.isInf(d)) return false;
        const t = @trunc(d);
        if (t != d) return false;
        if (t >= 18446744073709551616.0) return false; // >= 2^64
        if (t < 0) return false;
        return @as(u64, @intFromFloat(t)) == u;
    }

    fn numericEql(a: Value, b: Value) bool {
        switch (a) {
            .int_t => |i| switch (b) {
                .int_t => |j| return i == j,
                .uint_t => |u| return i >= 0 and @as(u64, @bitCast(i)) == u,
                .double_t => |d| return intDoubleEql(i, d),
                else => unreachable,
            },
            .uint_t => |u| switch (b) {
                .int_t => |i| return i >= 0 and @as(u64, @bitCast(i)) == u,
                .uint_t => |v| return u == v,
                .double_t => |d| return uintDoubleEql(u, d),
                else => unreachable,
            },
            .double_t => |d| switch (b) {
                .int_t => |i| return intDoubleEql(i, d),
                .uint_t => |u| return uintDoubleEql(u, d),
                .double_t => |e| return d == e, // NaN != NaN
                else => unreachable,
            },
            else => unreachable,
        }
    }

    // ---- ordering ----

    /// Cross-type ordering. Returns null when the values are incomparable
    /// (mismatched non-numeric types, or NaN involved).
    pub fn compare(a: Value, b: Value) ?std.math.Order {
        if (a.isNumeric() and b.isNumeric()) return numericCompare(a, b);
        if (@intFromEnum(std.meta.activeTag(a)) != @intFromEnum(std.meta.activeTag(b))) return null;
        switch (a) {
            .bool_t => |x| {
                const y = b.bool_t;
                if (x == y) return .eq;
                return if (!x) .lt else .gt;
            },
            .string_t => |x| return std.mem.order(u8, x, b.string_t),
            .bytes_t => |x| return std.mem.order(u8, x, b.bytes_t),
            .duration_t => |x| {
                const y = b.duration_t;
                if (x.seconds != y.seconds) return std.math.order(x.seconds, y.seconds);
                return std.math.order(x.nanos, y.nanos);
            },
            .timestamp_t => |x| {
                const y = b.timestamp_t;
                if (x.seconds != y.seconds) return std.math.order(x.seconds, y.seconds);
                return std.math.order(x.nanos, y.nanos);
            },
            else => return null,
        }
    }

    fn numericCompare(a: Value, b: Value) ?std.math.Order {
        switch (a) {
            .int_t => |i| switch (b) {
                .int_t => |j| return std.math.order(i, j),
                .uint_t => |u| {
                    if (i < 0) return .lt;
                    return std.math.order(@as(u64, @bitCast(i)), u);
                },
                .double_t => |d| return intDoubleCompare(i, d),
                else => unreachable,
            },
            .uint_t => |u| switch (b) {
                .int_t => |i| {
                    if (i < 0) return .gt;
                    return std.math.order(u, @as(u64, @bitCast(i)));
                },
                .uint_t => |v| return std.math.order(u, v),
                .double_t => |d| return uintDoubleCompare(u, d),
                else => unreachable,
            },
            .double_t => |d| switch (b) {
                .int_t => |i| return invert(intDoubleCompare(i, d)),
                .uint_t => |u| return invert(uintDoubleCompare(u, d)),
                .double_t => |e| {
                    if (std.math.isNan(d) or std.math.isNan(e)) return null;
                    return std.math.order(d, e);
                },
                else => unreachable,
            },
            else => unreachable,
        }
    }

    fn invert(o: ?std.math.Order) ?std.math.Order {
        return if (o) |x| switch (x) {
            .lt => .gt,
            .gt => .lt,
            .eq => .eq,
        } else null;
    }

    fn intDoubleCompare(i: i64, d: f64) ?std.math.Order {
        if (std.math.isNan(d)) return null;
        if (std.math.isInf(d)) return if (d > 0) .lt else .gt;
        const t = @trunc(d);
        if (t >= 9223372036854775808.0) return .lt; // i < 2^63 <= t
        if (t < -9223372036854775808.0) return .gt; // i >= -2^63 > t
        const ti: i64 = @intFromFloat(t);
        if (i < ti) return .lt;
        if (i > ti) return .gt;
        // i == t; fractional part of d decides.
        return if (d > t) .lt else .eq;
    }

    fn uintDoubleCompare(u: u64, d: f64) ?std.math.Order {
        if (std.math.isNan(d)) return null;
        if (std.math.isInf(d)) return if (d > 0) .lt else .gt;
        const t = @trunc(d);
        if (t >= 18446744073709551616.0) return .lt; // u < 2^64 <= t
        if (t < 0) return .gt;
        const tu: u64 = @intFromFloat(t);
        if (u < tu) return .lt;
        if (u > tu) return .gt;
        return if (d > t) .lt else .eq;
    }

    // ---- hashing (consistent with eql) ----

    /// Hash a value such that equal values (per `eql`) hash equally. Numeric
    /// values hash by their canonical position on the number line: small
    /// integers and integral doubles hash as the double; integers too large
    /// to be represented exactly as a double hash with a distinguishing tag
    /// (no double can equal them).
    pub fn hash(v: Value) u64 {
        switch (v) {
            .null_t => return 0x01,
            .bool_t => |b| return hashMix(0x02, @intFromBool(b)),
            .int_t => |i| {
                if (i >= -(1 << 53) and i <= (1 << 53)) return hashDouble(@floatFromInt(i));
                const d: f64 = @floatFromInt(i);
                // If i is exactly representable as a double, hash as the
                // double so that (int i) == (double d) implies equal hashes.
                if (d >= -9223372036854775808.0 and d < 9223372036854775808.0) {
                    if (@as(i64, @intFromFloat(d)) == i) return hashDouble(d);
                }
                return hashMix(0x03, @bitCast(i));
            },
            .uint_t => |u| {
                if (u <= (1 << 53)) return hashDouble(@floatFromInt(u));
                const d: f64 = @floatFromInt(u);
                if (@as(u64, @intFromFloat(d)) == u) return hashDouble(d);
                return hashMix(0x04, u);
            },
            .double_t => |d| return hashDouble(d),
            .string_t => |s| return std.hash.Wyhash.hash(0xCE1, s),
            .bytes_t => |s| return std.hash.Wyhash.hash(0xCEB, s),
            .list_t => |l| {
                var h: u64 = 0x11;
                for (l) |e| h = hashMix(h, hash(e));
                return h;
            },
            .map_t => |m| {
                // Order-independent.
                var h: u64 = 0x12;
                for (m.entries) |e| h ^= hashMix(hash(e.key), hash(e.value));
                return h;
            },
            .type_t => |t| return hashMix(0x13, @intFromEnum(t)),
            .duration_t => |d| return hashMix(0x14, @bitCast(d.seconds)) ^ hashMix(0, @as(u32, @bitCast(d.nanos))),
            .timestamp_t => |t| return hashMix(0x15, @bitCast(t.seconds)) ^ hashMix(1, t.nanos),
        }
    }

    fn hashDouble(d: f64) u64 {
        var x = d;
        if (x == 0) x = 0; // canonicalize -0.0
        if (std.math.isNan(x)) x = std.math.nan(f64);
        return hashMix(0x16, @bitCast(x));
    }

    fn hashMix(a: u64, b: u64) u64 {
        var z = a +% (b *% 0x9E3779B97F4A7C15);
        z ^= z >> 30;
        z *%= 0xBF58476D1CE4E5B9;
        z ^= z >> 27;
        z *%= 0x94D049BB133111EB;
        z ^= z >> 31;
        return z;
    }
};

pub fn durationToSeconds(d: Duration) f64 {
    return @as(f64, @floatFromInt(d.seconds)) + @as(f64, @floatFromInt(d.nanos)) / 1e9;
}

/// An insertion-ordered map with an open-addressing index.
/// `entries` preserves insertion order (used by comprehensions); `index`
/// maps hashed keys to entry slots. Lookup uses heterogeneous equality, so
/// `{1: 'a'}[1.0]` finds the int key `1`.
pub const Map = struct {
    pub const Entry = struct { key: Value, value: Value };

    entries: []Entry,
    /// Slot -> entry index + 1; 0 means empty.
    index: []u32,
    mask: u32,

    pub const empty = Map{ .entries = &.{}, .index = &.{}, .mask = 0 };

    /// Build a map from entry pairs. Returns error.RepeatedKey on duplicate
    /// keys (heterogeneously compared).
    pub fn init(alloc: std.mem.Allocator, entries: []Entry) error{ RepeatedKey, OutOfMemory }!*Map {
        const m = try alloc.create(Map);
        errdefer alloc.destroy(m);
        try m.build(alloc, entries);
        return m;
    }

    fn build(m: *Map, alloc: std.mem.Allocator, entries: []Entry) error{ RepeatedKey, OutOfMemory }!void {
        var cap: u32 = 8;
        while (cap < entries.len * 2) cap *= 2;
        const index = try alloc.alloc(u32, cap);
        @memset(index, 0);
        m.entries = entries;
        m.index = index;
        m.mask = cap - 1;
        for (entries, 0..) |e, i| {
            const h = Value.hash(e.key) & m.mask;
            var slot: usize = h;
            while (true) {
                const v = index[slot];
                if (v == 0) {
                    index[slot] = @intCast(i + 1);
                    break;
                }
                if (Value.eql(entries[v - 1].key, e.key)) return error.RepeatedKey;
                slot = (slot + 1) & m.mask;
            }
        }
    }

    /// Create the canonical empty map (shared, no allocation).
    pub fn emptyMap() *const Map {
        return &empty;
    }

    pub fn size(m: *const Map) usize {
        return m.entries.len;
    }

    pub fn get(m: *const Map, key: Value) ?Value {
        if (m.entries.len == 0) return null;
        var slot: usize = Value.hash(key) & m.mask;
        while (true) {
            const v = m.index[slot];
            if (v == 0) return null;
            const e = &m.entries[v - 1];
            if (Value.eql(e.key, key)) return e.value;
            slot = (slot + 1) & m.mask;
        }
    }

    pub fn has(m: *const Map, key: Value) bool {
        return m.get(key) != null;
    }
};

test "numeric equality" {
    const t = std.testing;
    try t.expect(Value.eql(.{ .int_t = 1 }, .{ .int_t = 1 }));
    try t.expect(Value.eql(.{ .int_t = 1 }, .{ .uint_t = 1 }));
    try t.expect(Value.eql(.{ .int_t = 1 }, .{ .double_t = 1.0 }));
    try t.expect(Value.eql(.{ .uint_t = 1 }, .{ .double_t = 1.0 }));
    try t.expect(!Value.eql(.{ .int_t = -1 }, .{ .uint_t = 18446744073709551615 }));
    try t.expect(!Value.eql(.{ .double_t = std.math.nan(f64) }, .{ .double_t = std.math.nan(f64) }));
    try t.expect(Value.eql(.{ .double_t = 0.0 }, .{ .double_t = -0.0 }));
    // Exact large-number comparisons.
    try t.expect(Value.eql(.{ .int_t = 1 << 53 }, .{ .double_t = 9007199254740992.0 }));
    try t.expect(!Value.eql(.{ .int_t = (1 << 53) + 1 }, .{ .double_t = 9007199254740992.0 }));
    try t.expect(Value.eql(.{ .int_t = (1 << 53) + 2 }, .{ .double_t = 9007199254740994.0 }));
    try t.expect(Value.eql(.{ .int_t = -5 }, .{ .double_t = -5.0 }));
    try t.expect(!Value.eql(.{ .int_t = 5 }, .{ .double_t = 5.5 }));
    // Type mismatch.
    try t.expect(!Value.eql(.{ .int_t = 1 }, .{ .string_t = "1" }));
    try t.expect(!Value.eql(.{ .bool_t = true }, .{ .int_t = 1 }));
}

test "ordering" {
    const t = std.testing;
    try t.expectEqual(std.math.Order.lt, Value.compare(.{ .int_t = 1 }, .{ .double_t = 1.5 }).?);
    try t.expectEqual(std.math.Order.eq, Value.compare(.{ .int_t = 1 }, .{ .double_t = 1.0 }).?);
    try t.expectEqual(std.math.Order.gt, Value.compare(.{ .uint_t = 2 }, .{ .int_t = 1 }).?);
    try t.expect(Value.compare(.{ .double_t = std.math.nan(f64) }, .{ .double_t = 1.0 }) == null);
    try t.expectEqual(std.math.Order.lt, Value.compare(.{ .bool_t = false }, .{ .bool_t = true }).?);
    try t.expectEqual(std.math.Order.lt, Value.compare(.{ .string_t = "a" }, .{ .string_t = "b" }).?);
    try t.expect(Value.compare(.{ .string_t = "a" }, .{ .int_t = 1 }) == null);
}

test "map" {
    const t = std.testing;
    var arena = std.heap.ArenaAllocator.init(t.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var entries = [_]Map.Entry{
        .{ .key = .{ .int_t = 1 }, .value = .{ .string_t = "one" } },
        .{ .key = .{ .string_t = "k" }, .value = .{ .int_t = 42 } },
        .{ .key = .{ .bool_t = true }, .value = .{ .uint_t = 7 } },
    };
    const m = try Map.init(a, &entries);
    try t.expectEqualStrings("one", m.get(.{ .int_t = 1 }).?.string_t);
    // Heterogeneous lookup.
    try t.expectEqualStrings("one", m.get(.{ .double_t = 1.0 }).?.string_t);
    try t.expectEqualStrings("one", m.get(.{ .uint_t = 1 }).?.string_t);
    try t.expectEqual(@as(i64, 42), m.get(.{ .string_t = "k" }).?.int_t);
    try t.expect(m.get(.{ .string_t = "nope" }) == null);
    // Duplicate detection (heterogeneous).
    var dup = [_]Map.Entry{
        .{ .key = .{ .int_t = 1 }, .value = .{ .string_t = "one" } },
        .{ .key = .{ .uint_t = 1 }, .value = .{ .string_t = "uno" } },
    };
    try t.expectError(error.RepeatedKey, Map.init(a, &dup));
}

test "hash consistency with equality" {
    const t = std.testing;
    const cases = .{
        .{ Value{ .int_t = 1 }, Value{ .double_t = 1.0 } },
        .{ Value{ .int_t = 1 }, Value{ .uint_t = 1 } },
        .{ Value{ .uint_t = 5 }, Value{ .double_t = 5.0 } },
        .{ Value{ .double_t = -0.0 }, Value{ .int_t = 0 } },
        .{ Value{ .int_t = (1 << 53) + 2 }, Value{ .double_t = 9007199254740994.0 } },
    };
    inline for (cases) |c| {
        try t.expect(Value.eql(c[0], c[1]));
        try t.expectEqual(Value.hash(c[0]), Value.hash(c[1]));
    }
}
