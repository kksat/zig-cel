//! Timestamp and duration support: RFC 3339 parsing/formatting, duration
//! string parsing ("1h30m", "-1.5s", ...), civil-calendar math, and a small
//! set of timezones (UTC, fixed offsets, and the named zones exercised by
//! the conformance suite with year-accurate US/AU DST rules).

const std = @import("std");
const value = @import("value.zig");

pub const Duration = value.Duration;
pub const Timestamp = value.Timestamp;

pub const Error = error{ Parse, OutOfMemory };

// ---- civil calendar (Howard Hinnant's algorithms) ----

/// Days since 1970-01-01 for the given civil date. m in [1,12], d in [1,31].
pub fn daysFromCivil(y_in: i64, m: i64, d: i64) i64 {
    var y = y_in;
    if (m <= 2) y -= 1;
    const era = @divFloor(if (y >= 0) y else y - 399, 400);
    const yoe = y - era * 400; // [0, 399]
    const doy = @divFloor(153 * (if (m > 2) m - 3 else m + 9) + 2, 5) + d - 1; // [0, 365]
    const doe = yoe * 365 + @divFloor(yoe, 4) - @divFloor(yoe, 100) + doy; // [0, 146096]
    return era * 146097 + doe - 719468;
}

pub const CivilDate = struct { year: i64, month: u32, day: u32 };

pub fn civilFromDays(z_in: i64) CivilDate {
    const z = z_in + 719468;
    const era = @divFloor(if (z >= 0) z else z - 146096, 146097);
    const doe = z - era * 146097; // [0, 146096]
    const yoe = @divFloor(doe - @divFloor(doe, 1460) + @divFloor(doe, 36524) - @divFloor(doe, 146096), 365); // [0, 399]
    const y = yoe + era * 400;
    const doy = doe - (365 * yoe + @divFloor(yoe, 4) - @divFloor(yoe, 100)); // [0, 365]
    const mp = @divFloor(5 * doy + 2, 153); // [0, 11]
    const d = doy - @divFloor(153 * mp + 2, 5) + 1; // [1, 31]
    const m = if (mp < 10) mp + 3 else mp - 9; // [1, 12]
    return .{ .year = if (m <= 2) y + 1 else y, .month = @intCast(m), .day = @intCast(d) };
}

pub fn isLeapYear(y: i64) bool {
    return @mod(y, 4) == 0 and (@mod(y, 100) != 0 or @mod(y, 400) == 0);
}

pub fn daysInMonth(y: i64, m: u32) u32 {
    return switch (m) {
        1, 3, 5, 7, 8, 10, 12 => 31,
        4, 6, 9, 11 => 30,
        2 => if (isLeapYear(y)) 29 else 28,
        else => 0,
    };
}

/// Day of week, 0 = Sunday.
pub fn dayOfWeek(days: i64) u32 {
    return @intCast(@mod(days + 4, 7));
}

/// Day of year, 0-based.
pub fn dayOfYear(days: i64) i64 {
    const c = civilFromDays(days);
    return days - daysFromCivil(c.year, c.month, 1);
}

// ---- timestamp ----

const TsParse = struct {
    year: i64,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
    second: u32,
    nanos: u32,
    offset_secs: i64, // parsed offset to subtract to get UTC
};

fn parseDigits(s: []const u8, i: *usize, count: usize) Error!u32 {
    if (i.* + count > s.len) return error.Parse;
    var v: u32 = 0;
    for (s[i.*..][0..count]) |c| {
        if (c < '0' or c > '9') return error.Parse;
        v = v * 10 + (c - '0');
    }
    i.* += count;
    return v;
}

fn parseFixedOffset(s: []const u8, i: *usize) Error!i64 {
    const sign: i64 = switch (s[i.*]) {
        '+' => 1,
        '-' => -1,
        else => return error.Parse,
    };
    i.* += 1;
    const hh = try parseDigits(s, i, 2);
    if (i.* >= s.len or s[i.*] != ':') return error.Parse;
    i.* += 1;
    const mm = try parseDigits(s, i, 2);
    if (hh > 23 or mm > 59) return error.Parse;
    return sign * (@as(i64, hh) * 3600 + @as(i64, mm) * 60);
}

/// Parse an RFC 3339 timestamp: `YYYY-MM-DDTHH:MM:SS[.fff...](Z|±HH:MM)`.
pub fn parseTimestamp(s: []const u8) Error!Timestamp {
    var i: usize = 0;
    if (s.len < 20) return error.Parse;
    const year = try parseDigits(s, &i, 4);
    if (s[i] != '-') return error.Parse;
    i += 1;
    const month = try parseDigits(s, &i, 2);
    if (s[i] != '-') return error.Parse;
    i += 1;
    const day = try parseDigits(s, &i, 2);
    if (s[i] != 'T' and s[i] != 't') return error.Parse;
    i += 1;
    const hour = try parseDigits(s, &i, 2);
    if (s[i] != ':') return error.Parse;
    i += 1;
    const minute = try parseDigits(s, &i, 2);
    if (s[i] != ':') return error.Parse;
    i += 1;
    const second = try parseDigits(s, &i, 2);

    if (month < 1 or month > 12) return error.Parse;
    if (day < 1 or day > daysInMonth(year, month)) return error.Parse;
    if (hour > 23 or minute > 59 or second > 59) return error.Parse;

    var nanos: u32 = 0;
    if (i < s.len and s[i] == '.') {
        i += 1;
        const frac_start = i;
        while (i < s.len and s[i] >= '0' and s[i] <= '9') i += 1;
        const frac_len = i - frac_start;
        if (frac_len == 0 or frac_len > 9) return error.Parse;
        var scale: u32 = 1;
        var k: usize = 0;
        while (k < 9 - frac_len) : (k += 1) scale *= 10;
        var fs: usize = frac_start;
        nanos = try parseDigits(s, &fs, frac_len) * scale;
    }

    var offset: i64 = 0;
    if (i >= s.len) return error.Parse;
    switch (s[i]) {
        'Z', 'z' => i += 1,
        '+', '-' => offset = try parseFixedOffset(s, &i),
        else => return error.Parse,
    }
    if (i != s.len) return error.Parse;

    const days = daysFromCivil(year, month, day);
    const secs = days * 86400 + @as(i64, hour) * 3600 + @as(i64, minute) * 60 + second - offset;
    if (secs < Timestamp.min_seconds or secs > Timestamp.max_seconds) return error.Parse;
    return .{ .seconds = secs, .nanos = nanos };
}

/// Format a timestamp as RFC 3339 with 'Z' and 0/3/6/9 fractional digits.
/// Caller owns the returned buffer (use a fixed 64-byte buffer).
pub fn formatTimestamp(t: Timestamp) [36]u8 {
    var buf: [36]u8 = undefined;
    var len: usize = 0;
    const days = @divFloor(t.seconds, 86400);
    const rem = t.seconds - days * 86400;
    const c = civilFromDays(days);
    len += writeDigits4(buf[len..], c.year);
    buf[len] = '-';
    len += 1;
    writeDigits2(buf[len..], c.month);
    len += 2;
    buf[len] = '-';
    len += 1;
    writeDigits2(buf[len..], c.day);
    len += 2;
    buf[len] = 'T';
    len += 1;
    writeDigits2(buf[len..], @intCast(@divFloor(rem, 3600)));
    len += 2;
    buf[len] = ':';
    len += 1;
    writeDigits2(buf[len..], @intCast(@mod(@divFloor(rem, 60), 60)));
    len += 2;
    buf[len] = ':';
    len += 1;
    writeDigits2(buf[len..], @intCast(@mod(rem, 60)));
    len += 2;

    if (t.nanos != 0) {
        buf[len] = '.';
        len += 1;
        var digits: u32 = t.nanos;
        var width: usize = 9;
        var trim: usize = 0;
        if (digits % 1000 == 0) {
            width = 6;
            trim = 3;
            digits /= 1000;
            if (digits % 1000 == 0) {
                width = 3;
                trim = 6;
                digits /= 1000;
            }
        }
        var tmp: [9]u8 = undefined;
        writeDigitsN(&tmp, digits, 9);
        @memcpy(buf[len .. len + width], tmp[trim..9]);
        len += width;
    }
    buf[len] = 'Z';
    len += 1;
    @memset(buf[len..], 0);
    return buf;
}

fn writeDigits4(buf: []u8, v: i64) usize {
    writeDigitsN(buf, @intCast(@mod(v, 10000)), 4);
    return 4;
}

fn writeDigits2(buf: []u8, v: u32) void {
    writeDigitsN(buf, v, 2);
}

fn writeDigitsN(buf: []u8, v_in: u32, width: usize) void {
    var v = v_in;
    var i: usize = width;
    while (i > 0) {
        i -= 1;
        buf[i] = @intCast('0' + v % 10);
        v /= 10;
    }
}

// ---- duration ----

const unit_table = [_]struct { suffix: []const u8, nanos: i128 }{
    .{ .suffix = "ns", .nanos = 1 },
    .{ .suffix = "us", .nanos = 1000 },
    // cel-go also accepts "µs" / "μs".
    .{ .suffix = "\xc2\xb5s", .nanos = 1000 },
    .{ .suffix = "\xce\xbcs", .nanos = 1000 },
    .{ .suffix = "ms", .nanos = 1000_000 },
    .{ .suffix = "s", .nanos = 1000_000_000 },
    .{ .suffix = "m", .nanos = 60_000_000_000 },
    .{ .suffix = "h", .nanos = 3600_000_000_000 },
};

/// Parse a duration string: sign? ({digits}[.{digits}]unit)+, e.g. "1h30m",
/// "-1.5s", "0". Returns nanoseconds as i128 (caller range-checks).
pub fn parseDurationNanos(s: []const u8) Error!i128 {
    if (s.len == 0) return error.Parse;
    // The zero duration may be written without a unit.
    if (std.mem.eql(u8, s, "0") or std.mem.eql(u8, s, "-0") or std.mem.eql(u8, s, "+0")) return 0;
    var i: usize = 0;
    var neg = false;
    if (s[i] == '-') {
        neg = true;
        i += 1;
    } else if (s[i] == '+') {
        i += 1;
    }
    if (i >= s.len) return error.Parse;

    var total: i128 = 0;
    var any = false;
    while (i < s.len) {
        const int_start = i;
        while (i < s.len and s[i] >= '0' and s[i] <= '9') i += 1;
        if (i == int_start) return error.Parse;
        var int_part: i128 = 0;
        for (s[int_start..i]) |c| {
            int_part = int_part * 10 + (c - '0');
            if (int_part > 1 << 100) return error.Parse; // absurd magnitude
        }
        var frac_num: i128 = 0;
        var frac_den: i128 = 1;
        if (i < s.len and s[i] == '.') {
            i += 1;
            const frac_start = i;
            while (i < s.len and s[i] >= '0' and s[i] <= '9') i += 1;
            if (i == frac_start) return error.Parse;
            for (s[frac_start..i]) |c| {
                if (frac_den < 1 << 100) {
                    frac_num = frac_num * 10 + (c - '0');
                    frac_den *= 10;
                }
            }
        }
        // Unit suffix.
        var unit_nanos: ?i128 = null;
        for (unit_table) |u| {
            if (s.len - i >= u.suffix.len and std.mem.eql(u8, s[i .. i + u.suffix.len], u.suffix)) {
                unit_nanos = u.nanos;
                i += u.suffix.len;
                break;
            }
        }
        const un = unit_nanos orelse return error.Parse;
        total += int_part * un;
        // Fraction: scale to nanos, dropping sub-nanosecond precision.
        total += @divFloor(frac_num * un, frac_den);
        any = true;
    }
    if (!any) return error.Parse;
    return if (neg) -total else total;
}

pub fn parseDuration(s: []const u8) Error!Duration {
    const nanos = try parseDurationNanos(s);
    return durationFromNanos(nanos);
}

pub fn durationFromNanos(nanos: i128) Error!Duration {
    const min: i128 = @as(i128, Duration.min_seconds) * 1_000_000_000 - 999_999_999;
    const max: i128 = @as(i128, Duration.max_seconds) * 1_000_000_000 + 999_999_999;
    if (nanos < min or nanos > max) return error.Parse;
    // Truncate toward zero so that, for negative durations, the nanoseconds
    // part carries the sign (proto normalization).
    const secs = @divTrunc(nanos, 1_000_000_000);
    const ns = nanos - secs * 1_000_000_000;
    return .{ .seconds = @intCast(secs), .nanos = @intCast(ns) };
}

pub fn durationToNanos(d: Duration) i128 {
    return @as(i128, d.seconds) * 1_000_000_000 + d.nanos;
}

/// Format a duration as CEL's string form: seconds with up to 9 fractional
/// digits and an 's' suffix (e.g. "60.001s", "-90s").
pub fn formatDuration(d: Duration, w: *std.Io.Writer) std.Io.Writer.Error!void {
    var buf: [48]u8 = undefined;
    var fbs = std.Io.Writer.fixed(&buf);
    const w2 = &fbs;
    var total = durationToNanos(d);
    if (total == 0) {
        try w.writeAll("0s");
        return;
    }
    if (total < 0) {
        try w2.writeByte('-');
        total = -total;
    }
    const secs = @divFloor(total, 1_000_000_000);
    const ns: u64 = @intCast(total - secs * 1_000_000_000);
    try w2.printInt(@as(u64, @intCast(secs)), 10, .lower, .{});
    if (ns != 0) {
        try w2.writeByte('.');
        var digits: [9]u8 = undefined;
        var v = ns;
        var k: usize = 9;
        while (k > 0) {
            k -= 1;
            digits[k] = @intCast('0' + v % 10);
            v /= 10;
        }
        var width: usize = 9;
        while (width > 0 and digits[width - 1] == '0') width -= 1;
        try w2.writeAll(digits[0..width]);
    }
    try w2.writeByte('s');
    try w.writeAll(fbs.buffered());
}

// ---- timezones ----

pub const TimeZone = struct {
    /// Offset from UTC in seconds at the given timestamp instant.
    offset_secs: i64,

    pub fn resolve(name: []const u8, ts_seconds: i64) Error!TimeZone {
        if (name.len == 0) return error.Parse;
        if (std.mem.eql(u8, name, "UTC")) return .{ .offset_secs = 0 };
        // Fixed offset ±HH:MM.
        if (name[0] == '+' or name[0] == '-') {
            var i: usize = 1;
            const hh = try parseDigits(name, &i, 2);
            if (name[i] != ':') return error.Parse;
            i += 1;
            const mm = try parseDigits(name, &i, 2);
            if (i != name.len or hh > 23 or mm > 59) return error.Parse;
            const sign: i64 = if (name[0] == '+') 1 else -1;
            return .{ .offset_secs = sign * (@as(i64, hh) * 3600 + @as(i64, mm) * 60) };
        }
        // Named zones (subset with DST rules accurate for modern dates).
        const zone_info = struct {
            name: []const u8,
            std_offset: i64,
            dst_offset: i64,
            /// DST period: southern=true means DST spans Oct..Apr.
            southern: bool,
        };
        const zones = [_]zone_info{
            .{ .name = "America/St_Johns", .std_offset = -3 * 3600 - 1800, .dst_offset = -2 * 3600 - 1800, .southern = false },
            .{ .name = "US/Central", .std_offset = -6 * 3600, .dst_offset = -5 * 3600, .southern = false },
            .{ .name = "US/Eastern", .std_offset = -5 * 3600, .dst_offset = -4 * 3600, .southern = false },
            .{ .name = "US/Pacific", .std_offset = -8 * 3600, .dst_offset = -7 * 3600, .southern = false },
            .{ .name = "America/Los_Angeles", .std_offset = -8 * 3600, .dst_offset = -7 * 3600, .southern = false },
            .{ .name = "America/New_York", .std_offset = -5 * 3600, .dst_offset = -4 * 3600, .southern = false },
            .{ .name = "Europe/Paris", .std_offset = 1 * 3600, .dst_offset = 2 * 3600, .southern = false },
            .{ .name = "Europe/London", .std_offset = 0, .dst_offset = 1 * 3600, .southern = false },
            .{ .name = "Australia/Sydney", .std_offset = 10 * 3600, .dst_offset = 11 * 3600, .southern = true },
            .{ .name = "Asia/Kathmandu", .std_offset = 5 * 3600 + 2700, .dst_offset = 5 * 3600 + 2700, .southern = false },
            .{ .name = "Asia/Tokyo", .std_offset = 9 * 3600, .dst_offset = 9 * 3600, .southern = false },
            .{ .name = "CET", .std_offset = 1 * 3600, .dst_offset = 2 * 3600, .southern = false },
        };
        for (zones) |z| {
            if (!std.mem.eql(u8, z.name, name)) continue;
            if (z.dst_offset == z.std_offset) return .{ .offset_secs = z.std_offset };
            // DST determination in local standard time.
            const local = ts_seconds + z.std_offset;
            const days = @divFloor(local, 86400);
            const c = civilFromDays(days);
            if (z.southern) {
                // DST from first Sunday of October to first Sunday of April.
                if (inDSTSouthern(c.year, days)) return .{ .offset_secs = z.dst_offset };
            } else {
                // DST from second Sunday of March to first Sunday of November.
                if (inDSTNorthern(c.year, days)) return .{ .offset_secs = z.dst_offset };
            }
            return .{ .offset_secs = z.std_offset };
        }
        return error.Parse;
    }
};

/// Nth Sunday of month (1-based) as days-since-epoch.
fn nthSunday(year: i64, month: u32, n: u32) i64 {
    const first = daysFromCivil(year, month, 1);
    const dow = dayOfWeek(first); // 0 = Sunday
    const shift: i64 = @intCast((7 - dow) % 7);
    return first + shift + (@as(i64, n) - 1) * 7;
}

fn inDSTNorthern(year: i64, days: i64) bool {
    const start = nthSunday(year, 3, 2); // 2nd Sunday of March
    const end = nthSunday(year, 11, 1); // 1st Sunday of November
    return days >= start and days < end;
}

fn inDSTSouthern(year: i64, days: i64) bool {
    // DST from 1st Sunday of October (of year-1 or year) to 1st Sunday of April.
    const start_this = nthSunday(year, 10, 1);
    const end_this = nthSunday(year, 4, 1);
    return days >= start_this or days < end_this;
}

// ---- field accessors ----

pub const TsParts = struct {
    year: i64,
    month: u32, // 1-12
    day: u32, // 1-31
    hour: u32,
    minute: u32,
    second: u32,
    millis: u32,
    day_of_week: u32, // 0 = Sunday
    day_of_year: i64, // 0-based
};

/// Break a timestamp into components in the given timezone.
pub fn tsParts(t: Timestamp, tz: TimeZone) TsParts {
    const local = t.seconds + tz.offset_secs;
    const days = @divFloor(local, 86400);
    const rem = local - days * 86400;
    const c = civilFromDays(days);
    return .{
        .year = c.year,
        .month = c.month,
        .day = c.day,
        .hour = @intCast(@divFloor(rem, 3600)),
        .minute = @intCast(@mod(@divFloor(rem, 60), 60)),
        .second = @intCast(@mod(rem, 60)),
        .millis = t.nanos / 1_000_000,
        .day_of_week = dayOfWeek(days),
        .day_of_year = dayOfYear(days),
    };
}

test "civil calendar" {
    const t = std.testing;
    try t.expectEqual(@as(i64, 0), daysFromCivil(1970, 1, 1));
    try t.expectEqual(@as(i64, 1234567890 / 86400), daysFromCivil(2009, 2, 13));
    const c = civilFromDays(1234567890 / 86400);
    try t.expectEqual(@as(i64, 2009), c.year);
    try t.expectEqual(@as(u32, 2), c.month);
    try t.expectEqual(@as(u32, 13), c.day);
    // 1970-01-01 was a Thursday.
    try t.expectEqual(@as(u32, 4), dayOfWeek(0));
}

test "parse timestamps" {
    const t = std.testing;
    const ts = try parseTimestamp("2009-02-13T23:31:30Z");
    try t.expectEqual(@as(i64, 1234567890), ts.seconds);
    try t.expectEqual(@as(u32, 0), ts.nanos);

    const with_frac = try parseTimestamp("9999-12-31T23:59:59.999999999Z");
    try t.expectEqual(@as(i64, 253402300799), with_frac.seconds);
    try t.expectEqual(@as(u32, 999999999), with_frac.nanos);

    const offset = try parseTimestamp("2009-02-13T23:31:30+05:30");
    try t.expectEqual(@as(i64, 1234567890 - 5 * 3600 - 30 * 60), offset.seconds);

    const lowercase = try parseTimestamp("2009-02-13t23:31:30z");
    try t.expectEqual(@as(i64, 1234567890), lowercase.seconds);

    try t.expectError(error.Parse, parseTimestamp("2009-02-13T23:31:60Z"));
    try t.expectError(error.Parse, parseTimestamp("2009-02-30T00:00:00Z"));
    try t.expectError(error.Parse, parseTimestamp("0000-01-01T00:00:00Z"));
    try t.expectError(error.Parse, parseTimestamp("2009-02-13T23:31:30"));
    try t.expectError(error.Parse, parseTimestamp("2009-02-13 23:31:30Z"));
    try t.expectError(error.Parse, parseTimestamp("10000-01-01T00:00:00Z"));
}

test "format timestamps" {
    const t = std.testing;
    const cases = .{
        .{ "2009-02-13T23:31:30Z", "2009-02-13T23:31:30Z" },
        .{ "9999-12-31T23:59:59.999999999Z", "9999-12-31T23:59:59.999999999Z" },
        .{ "2020-01-01T00:00:00.500Z", "2020-01-01T00:00:00.500Z" },
        .{ "2020-01-01T00:00:00.000500Z", "2020-01-01T00:00:00.000500Z" },
    };
    inline for (cases) |c| {
        const ts = try parseTimestamp(c[0]);
        const buf = formatTimestamp(ts);
        try t.expectEqualStrings(c[1], std.mem.sliceTo(&buf, 0));
    }
}

test "parse durations" {
    const t = std.testing;
    const cases = .{
        .{ "0", 0 },
        .{ "1s", 1000000000 },
        .{ "1h", 3600000000000 },
        .{ "1h30m", 5400000000000 },
        .{ "-1.5h", -5400000000000 },
        .{ "1m1ms", 60001000000 },
        .{ "315576000000s999999999ns", 315576000000999999999 },
        .{ "-315576000000s999999999ns", -315576000000999999999 },
        .{ "23.4s", 23400000000 },
        .{ "1us", 1000 },
        .{ "1ms", 1000000 },
    };
    inline for (cases) |c| {
        const d = try parseDuration(c[0]);
        try t.expectEqual(c[1], durationToNanos(d));
    }
    try t.expectError(error.Parse, parseDuration(""));
    try t.expectError(error.Parse, parseDuration("1"));
    try t.expectError(error.Parse, parseDuration("s"));
    try t.expectError(error.Parse, parseDuration("1d"));
    try t.expectError(error.Parse, parseDuration("315576000001s"));
    try t.expectError(error.Parse, parseDuration("-0.5"));
}

test "timezone resolution" {
    const t = std.testing;
    // 2009-02-13T23:31:30Z; Sydney is UTC+11 (DST, southern summer).
    const syd = try TimeZone.resolve("Australia/Sydney", 1234567890);
    try t.expectEqual(@as(i64, 11 * 3600), syd.offset_secs);
    // US/Central in February: standard time, UTC-6.
    const central = try TimeZone.resolve("US/Central", 1234567890);
    try t.expectEqual(@as(i64, -6 * 3600), central.offset_secs);
    const st_johns = try TimeZone.resolve("America/St_Johns", 1234567890);
    try t.expectEqual(@as(i64, -3 * 3600 - 1800), st_johns.offset_secs);
    const kathmandu = try TimeZone.resolve("Asia/Kathmandu", 1234567890);
    try t.expectEqual(@as(i64, 5 * 3600 + 2700), kathmandu.offset_secs);
    // July 2009: US DST active, Sydney standard.
    const july_days = daysFromCivil(2009, 7, 1);
    const july = july_days * 86400;
    const central_summer = try TimeZone.resolve("US/Central", july);
    try t.expectEqual(@as(i64, -5 * 3600), central_summer.offset_secs);
    const syd_winter = try TimeZone.resolve("Australia/Sydney", july);
    try t.expectEqual(@as(i64, 10 * 3600), syd_winter.offset_secs);
    // Fixed offsets.
    const fixed = try TimeZone.resolve("-09:30", 1234567890);
    try t.expectEqual(@as(i64, -9 * 3600 - 1800), fixed.offset_secs);
}
