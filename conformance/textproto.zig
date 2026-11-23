//! Minimal textproto reader for the cel-spec simple conformance suite.
//!
//! This is a schema-specific parser for `cel.expr.conformance.test.
//! SimpleTestFile` and the `cel.expr.Value` / `cel.expr.ExprValue` messages
//! it references -- just enough to run the vendored `.textproto` test data
//! (including `google.protobuf.Duration`/`Timestamp` `object_value`s and
//! `Any`-style `[type.googleapis.com/...]` annotations). Everything else
//! (type_env, enums, other object types) is skipped or marked unsupported.

const std = @import("std");

pub const Error = error{ Parse, OutOfMemory };

/// Position of the last parse error (diagnostic aid).
pub var last_err_pos: usize = 0;
pub var last_err_msg: []const u8 = "";

pub const TValue = union(enum) {
    unset: void,
    null_v: void,
    bool_v: bool,
    int_v: i64,
    uint_v: u64,
    double_v: f64,
    string_v: []const u8,
    bytes_v: []const u8,
    list_v: []const TValue,
    map_v: []const TEntry,
    type_v: []const u8,
    /// google.protobuf.Duration object_value.
    obj_duration: struct { seconds: i64, nanos: i32 },
    /// google.protobuf.Timestamp object_value.
    obj_timestamp: struct { seconds: i64, nanos: i32 },
    /// Any other object_value: unsupported (test is skipped).
    obj_other: []const u8,
    enum_v: void,
};

pub const TEntry = struct {
    key: TValue,
    value: TValue,
};

pub const ExprValue = union(enum) {
    val: TValue,
    err: []const []const u8,
    unknown: void,
};

pub const Test = struct {
    name: []const u8,
    expr: []const u8,
    disable_macros: bool = false,
    check_only: bool = false,
    container: []const u8 = "",
    bindings: []const Binding = &.{},
    /// Result matcher. `default` means "no matcher specified", which the
    /// suite defines as expecting the boolean true.
    result: Result = .default_true,
};

pub const Binding = struct {
    name: []const u8,
    value: ExprValue,
};

pub const Result = union(enum) {
    value: TValue,
    eval_error: []const []const u8,
    unknown: void,
    default_true: void,
};

pub const Section = struct {
    name: []const u8,
    tests: []const Test,
};

pub const File = struct {
    name: []const u8,
    sections: []const Section,
};

const Lexer = struct {
    src: []const u8,
    pos: usize = 0,
    alloc: std.mem.Allocator,
    err_msg: []const u8 = "",

    const Token = union(enum) {
        ident: []const u8,
        string: []const u8, // cooked
        int: i64,
        uint: u64,
        double: f64,
        bool: bool,
        lbrace: void,
        rbrace: void,
        langle: void,
        rangle: void,
        colon: void,
        comma: void,
        url: []const u8, // [type.googleapis.com/...]
        eof: void,
    };

    fn fail(lex: *Lexer, msg: []const u8) Error {
        lex.err_msg = msg;
        last_err_pos = lex.pos;
        last_err_msg = msg;
        return error.Parse;
    }

    fn skipSpace(lex: *Lexer) void {
        while (lex.pos < lex.src.len) {
            const c = lex.src[lex.pos];
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                lex.pos += 1;
            } else if (c == '#') {
                while (lex.pos < lex.src.len and lex.src[lex.pos] != '\n') lex.pos += 1;
            } else break;
        }
    }

    fn next(lex: *Lexer) Error!Token {
        lex.skipSpace();
        if (lex.pos >= lex.src.len) return .eof;
        const c = lex.src[lex.pos];
        switch (c) {
            '{' => {
                lex.pos += 1;
                return .lbrace;
            },
            '}' => {
                lex.pos += 1;
                return .rbrace;
            },
            '<' => {
                lex.pos += 1;
                return .langle;
            },
            '>' => {
                lex.pos += 1;
                return .rangle;
            },
            ':' => {
                lex.pos += 1;
                return .colon;
            },
            ',' => {
                lex.pos += 1;
                return .comma;
            },
            '[' => {
                // Any URL annotation.
                const end = std.mem.indexOfScalarPos(u8, lex.src, lex.pos, ']') orelse return lex.fail("unterminated [url]");
                const url = lex.src[lex.pos + 1 .. end];
                lex.pos = end + 1;
                return .{ .url = url };
            },
            '"', '\'' => return .{ .string = try lex.lexString() },
            '-', '+', '.', '0'...'9' => return lex.lexNumber(),
            'a'...'z', 'A'...'Z', '_' => {
                const start = lex.pos;
                while (lex.pos < lex.src.len) {
                    const ch = lex.src[lex.pos];
                    if ((ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or (ch >= '0' and ch <= '9') or ch == '_' or ch == '.') {
                        lex.pos += 1;
                    } else break;
                }
                const word = lex.src[start..lex.pos];
                // Byte (`b`/`B`) and raw (`r`/`R`) string prefixes.
                if (word.len <= 2 and lex.pos < lex.src.len and (lex.src[lex.pos] == '\'' or lex.src[lex.pos] == '"')) {
                    var raw = false;
                    var ok = word.len > 0;
                    for (word) |ch| {
                        switch (ch) {
                            'r', 'R' => raw = true,
                            'b', 'B' => {},
                            else => ok = false,
                        }
                    }
                    if (ok) {
                        const s = try lex.lexString();
                        if (raw) return .{ .string = s }; // raw: escapes already literal
                        return .{ .string = s };
                    }
                }
                if (std.mem.eql(u8, word, "true")) return .{ .bool = true };
                if (std.mem.eql(u8, word, "false")) return .{ .bool = false };
                // Textproto float literals (case-insensitive).
                var lower_buf: [16]u8 = undefined;
                if (word.len <= lower_buf.len) {
                    for (word, 0..) |ch, k| lower_buf[k] = std.ascii.toLower(ch);
                    const lower = lower_buf[0..word.len];
                    if (std.mem.eql(u8, lower, "nan")) return .{ .double = std.math.nan(f64) };
                    if (std.mem.eql(u8, lower, "inf") or std.mem.eql(u8, lower, "infinity")) {
                        return .{ .double = std.math.inf(f64) };
                    }
                }
                return .{ .ident = word };
            },
            else => return lex.fail("unexpected character in textproto"),
        }
    }

    fn peek(lex: *Lexer) Error!Token {
        const save = lex.pos;
        const t = try lex.next();
        lex.pos = save;
        return t;
    }

    fn lexString(lex: *Lexer) Error![]const u8 {
        const quote = lex.src[lex.pos];
        lex.pos += 1;
        var buf = std.ArrayList(u8){};
        errdefer buf.deinit(lex.alloc);
        while (true) {
            if (lex.pos >= lex.src.len) return lex.fail("unterminated string");
            const c = lex.src[lex.pos];
            if (c == quote) {
                lex.pos += 1;
                return buf.toOwnedSlice(lex.alloc);
            }
            if (c == '\\') {
                lex.pos += 1;
                try lex.appendEscape(&buf);
                continue;
            }
            try buf.append(lex.alloc, c);
            lex.pos += 1;
        }
    }

    fn lexStringRaw(lex: *Lexer) Error![]const u8 {
        const quote = lex.src[lex.pos];
        lex.pos += 1;
        const start = lex.pos;
        while (lex.pos < lex.src.len and lex.src[lex.pos] != quote) lex.pos += 1;
        if (lex.pos >= lex.src.len) return lex.fail("unterminated string");
        const s = lex.src[start..lex.pos];
        lex.pos += 1;
        return s;
    }

    fn hexVal(c: u8) ?u8 {
        return switch (c) {
            '0'...'9' => c - '0',
            'a'...'f' => c - 'a' + 10,
            'A'...'F' => c - 'A' + 10,
            else => null,
        };
    }

    fn appendEscape(lex: *Lexer, buf: *std.ArrayList(u8)) Error!void {
        if (lex.pos >= lex.src.len) return lex.fail("bad escape");
        const c = lex.src[lex.pos];
        switch (c) {
            'n', 't', 'r', 'a', 'b', 'f', 'v', '\\', '"', '\'', '?' => {
                const out: u8 = switch (c) {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    'a' => 0x07,
                    'b' => 0x08,
                    'f' => 0x0c,
                    'v' => 0x0b,
                    else => c,
                };
                try buf.append(lex.alloc, out);
                lex.pos += 1;
            },
            'x', 'X' => {
                lex.pos += 1;
                if (lex.pos + 1 >= lex.src.len) return lex.fail("bad hex escape");
                const h1 = hexVal(lex.src[lex.pos]) orelse return lex.fail("bad hex escape");
                const h2 = hexVal(lex.src[lex.pos + 1]) orelse return lex.fail("bad hex escape");
                try buf.append(lex.alloc, h1 * 16 + h2);
                lex.pos += 2;
            },
            'u' => {
                lex.pos += 1;
                var v: u21 = 0;
                var i: usize = 0;
                while (i < 4) : (i += 1) {
                    const h: u21 = hexVal(lex.src[lex.pos + i]) orelse return lex.fail("bad unicode escape");
                    v = v * 16 + h;
                }
                lex.pos += 4;
                var tmp: [4]u8 = undefined;
                const n = std.unicode.utf8Encode(v, &tmp) catch return lex.fail("bad code point");
                try buf.appendSlice(lex.alloc, tmp[0..n]);
            },
            'U' => {
                lex.pos += 1;
                var v: u21 = 0;
                var i: usize = 0;
                while (i < 8) : (i += 1) {
                    const h: u21 = hexVal(lex.src[lex.pos + i]) orelse return lex.fail("bad unicode escape");
                    v = v * 16 + h;
                }
                lex.pos += 8;
                var tmp: [4]u8 = undefined;
                const n = std.unicode.utf8Encode(v, &tmp) catch return lex.fail("bad code point");
                try buf.appendSlice(lex.alloc, tmp[0..n]);
            },
            '0'...'7' => {
                // Octal escape: up to 3 digits.
                var v: u32 = 0;
                var i: usize = 0;
                while (i < 3 and lex.pos + i < lex.src.len) : (i += 1) {
                    const d = lex.src[lex.pos + i];
                    if (d < '0' or d > '7') break;
                    v = v * 8 + (d - '0');
                }
                lex.pos += i;
                try buf.append(lex.alloc, @intCast(v & 0xff));
            },
            else => return lex.fail("bad escape"),
        }
    }

    fn lexNumber(lex: *Lexer) Error!Token {
        const start = lex.pos;
        if (lex.src[lex.pos] == '-' or lex.src[lex.pos] == '+') lex.pos += 1;
        // inf / nan literals (possibly signed).
        if (lex.pos < lex.src.len and (lex.src[lex.pos] == 'i' or lex.src[lex.pos] == 'n')) {
            const wstart = lex.pos;
            while (lex.pos < lex.src.len) {
                const ch = lex.src[lex.pos];
                if ((ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z')) lex.pos += 1 else break;
            }
            const word = lex.src[wstart..lex.pos];
            const signed = lex.src[start] == '-';
            var lower_buf: [16]u8 = undefined;
            if (word.len <= lower_buf.len) {
                for (word, 0..) |ch, k| lower_buf[k] = std.ascii.toLower(ch);
                const lower = lower_buf[0..word.len];
                if (std.mem.eql(u8, lower, "inf") or std.mem.eql(u8, lower, "infinity")) {
                    return .{ .double = if (signed) -std.math.inf(f64) else std.math.inf(f64) };
                }
                if (std.mem.eql(u8, lower, "nan")) {
                    return .{ .double = std.math.nan(f64) };
                }
            }
            return lex.fail("bad number literal");
        }
        var is_double = false;
        while (lex.pos < lex.src.len) {
            const c = lex.src[lex.pos];
            if (c >= '0' and c <= '9') {
                lex.pos += 1;
            } else if (c == '.' or c == 'e' or c == 'E') {
                is_double = true;
                lex.pos += 1;
                if (lex.pos < lex.src.len and (lex.src[lex.pos] == '+' or lex.src[lex.pos] == '-')) lex.pos += 1;
            } else if ((c == 'x' or c == 'X') and lex.pos == start + 1 and lex.src[start] == '0') {
                is_double = false;
                lex.pos += 1;
                while (lex.pos < lex.src.len and hexVal(lex.src[lex.pos]) != null) lex.pos += 1;
                const v = std.fmt.parseInt(i64, lex.src[start + 2 .. lex.pos], 16) catch return lex.fail("bad hex number");
                return .{ .int = v };
            } else break;
        }
        const text = lex.src[start..lex.pos];
        if (is_double) {
            const v = std.fmt.parseFloat(f64, text) catch return lex.fail("bad float");
            return .{ .double = v };
        }
        if (std.mem.indexOfScalar(u8, text, '-') != null) {
            const v = std.fmt.parseInt(i64, text, 10) catch return lex.fail("bad int");
            return .{ .int = v };
        }
        const v = std.fmt.parseInt(u64, text, 10) catch return lex.fail("bad uint");
        if (v > std.math.maxInt(i64)) return .{ .uint = v };
        return .{ .int = @intCast(v) };
    }
};

pub const Token0 = Lexer.Token;

pub const Parser = struct {
    lex: Lexer,
    alloc: std.mem.Allocator,
    skipped_unsupported: bool = false,

    pub fn parse(alloc: std.mem.Allocator, src: []const u8) Error!File {
        var pr = Parser{ .lex = .{ .src = src, .alloc = alloc }, .alloc = alloc };
        last_err_pos = 0;
        return pr.parseFile() catch |e| {
            if (last_err_pos == 0) last_err_pos = pr.lex.pos;
            return e;
        };
    }

    fn next(p: *Parser) Error!Lexer.Token {
        return p.lex.next();
    }

    /// Textproto allows optional commas after field values.
    fn skipCommas(p: *Parser) Error!void {
        while (try p.lex.peek() == .comma) _ = try p.next();
    }

    /// Read a string value, concatenating adjacent string literals
    /// (textproto implicit concatenation across lines).
    fn readString(p: *Parser) Error![]const u8 {
        var buf = std.ArrayList(u8){};
        const first = try p.next();
        if (first != .string) return error.Parse;
        try buf.appendSlice(p.alloc, first.string);
        while (true) {
            const t = try p.lex.peek();
            if (t != .string) break;
            const s = try p.next();
            try buf.appendSlice(p.alloc, s.string);
        }
        return buf.toOwnedSlice(p.alloc);
    }

    fn expectIdent(p: *Parser, want: []const u8) Error!void {
        const t = try p.next();
        if (t != .ident or !std.mem.eql(u8, t.ident, want)) return error.Parse;
    }

    fn parseFile(p: *Parser) Error!File {
        var name: []const u8 = "";
        var sections = std.ArrayList(Section){};
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .eof) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "name")) {
                _ = try p.next(); // ':'
                name = try p.readString();
            } else if (std.mem.eql(u8, t.ident, "description")) {
                try p.skipFieldValue();
            } else if (std.mem.eql(u8, t.ident, "section")) {
                try sections.append(p.alloc, try p.parseSection());
            } else {
                try p.skipFieldValue();
            }
        }
        return .{ .name = name, .sections = try sections.toOwnedSlice(p.alloc) };
    }

    fn parseSection(p: *Parser) Error!Section {
        var name: []const u8 = "";
        var tests = std.ArrayList(Test){};
        const lead = try p.lex.peek();
        if (lead == .colon) _ = try p.next();
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "name")) {
                _ = try p.next();
                name = try p.readString();
            } else if (std.mem.eql(u8, t.ident, "test")) {
                try tests.append(p.alloc, try p.parseTest());
            } else {
                try p.skipFieldValue();
            }
        }
        return .{ .name = name, .tests = try tests.toOwnedSlice(p.alloc) };
    }

    fn parseTest(p: *Parser) Error!Test {
        var out_test = Test{ .name = "", .expr = "" };
        var bindings = std.ArrayList(Binding){};
        var result: Result = .default_true;
        const lead = try p.lex.peek();
        if (lead == .colon) _ = try p.next();
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            const field = t.ident;
            if (std.mem.eql(u8, field, "name")) {
                _ = try p.next();
                out_test.name = try p.readString();
            } else if (std.mem.eql(u8, field, "description")) {
                try p.skipFieldValue();
            } else if (std.mem.eql(u8, field, "expr")) {
                _ = try p.next();
                out_test.expr = try p.readString();
            } else if (std.mem.eql(u8, field, "disable_macros")) {
                _ = try p.next();
                out_test.disable_macros = (try p.next()).bool;
            } else if (std.mem.eql(u8, field, "disable_check") or std.mem.eql(u8, field, "check_only")) {
                _ = try p.next();
                const v = try p.next();
                if (std.mem.eql(u8, field, "check_only")) out_test.check_only = v.bool;
            } else if (std.mem.eql(u8, field, "container")) {
                _ = try p.next();
                out_test.container = try p.readString();
            } else if (std.mem.eql(u8, field, "bindings")) {
                try bindings.append(p.alloc, try p.parseBinding());
            } else if (std.mem.eql(u8, field, "value")) {
                result = .{ .value = try p.parseValueMessage() };
            } else if (std.mem.eql(u8, field, "typed_result")) {
                // result + deduced type; take the result value.
                result = .{ .value = try p.parseTypedResult() };
            } else if (std.mem.eql(u8, field, "eval_error")) {
                result = .{ .eval_error = try p.parseErrorSet() };
            } else if (std.mem.eql(u8, field, "any_eval_errors")) {
                // One of several error sets; take the first.
                result = .{ .eval_error = try p.parseAnyErrorSet() };
            } else if (std.mem.eql(u8, field, "unknown") or std.mem.eql(u8, field, "any_unknowns")) {
                result = .unknown;
                p.skipped_unsupported = true;
            } else if (std.mem.eql(u8, field, "locale")) {
                try p.skipFieldValue();
            } else {
                // type_env and anything else.
                try p.skipFieldValue();
            }
        }
        out_test.bindings = try bindings.toOwnedSlice(p.alloc);
        out_test.result = result;
        return out_test;
    }

    fn parseBinding(p: *Parser) Error!Binding {
        // bindings { key: "name" value { ... } }
        var name: []const u8 = "";
        var value: ExprValue = .{ .val = .unset };
        const lead = try p.lex.peek();
        if (lead == .colon) _ = try p.next();
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "key")) {
                _ = try p.next();
                name = try p.readString();
            } else if (std.mem.eql(u8, t.ident, "value")) {
                value = try p.parseExprValue();
            } else {
                try p.skipFieldValue();
            }
        }
        return .{ .name = name, .value = value };
    }

    fn parseExprValue(p: *Parser) Error!ExprValue {
        const lead = try p.lex.peek();
        if (lead == .colon) _ = try p.next();
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        var out: ExprValue = .{ .val = .unset };
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "value")) {
                out = .{ .val = try p.parseValueMessage() };
            } else if (std.mem.eql(u8, t.ident, "error")) {
                out = .{ .err = try p.parseErrorSet() };
            } else if (std.mem.eql(u8, t.ident, "unknown")) {
                out = .unknown;
            } else {
                try p.skipFieldValue();
            }
        }
        return out;
    }

    /// After `value:`/`value {` (colon optional before messages).
    fn parseValueMessage(p: *Parser) Error!TValue {
        const t = try p.lex.peek();
        if (t == .colon) _ = try p.next();
        return p.parseValueBody();
    }

    fn parseTypedResult(p: *Parser) Error!TValue {
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        var out: TValue = .unset;
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "result")) {
                out = try p.parseValueMessage();
            } else {
                try p.skipFieldValue();
            }
        }
        return out;
    }

    fn parseValueBody(p: *Parser) Error!TValue {
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        var out: TValue = .unset;
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            const field = t.ident;
            if (std.mem.eql(u8, field, "null_value")) {
                _ = try p.next(); // ':'
                _ = try p.next(); // NULL_VALUE / 0 / NullValue
                out = .null_v;
            } else if (std.mem.eql(u8, field, "bool_value")) {
                _ = try p.next();
                out = .{ .bool_v = (try p.next()).bool };
            } else if (std.mem.eql(u8, field, "int64_value")) {
                _ = try p.next();
                out = .{ .int_v = (try p.next()).int };
            } else if (std.mem.eql(u8, field, "uint64_value")) {
                _ = try p.next();
                const v = try p.next();
                out = switch (v) {
                    .int => |i| .{ .uint_v = @intCast(i) },
                    .uint => |u| .{ .uint_v = u },
                    else => return error.Parse,
                };
            } else if (std.mem.eql(u8, field, "double_value")) {
                _ = try p.next();
                const v = try p.next();
                out = switch (v) {
                    .double => |d| .{ .double_v = d },
                    .int => |i| .{ .double_v = @floatFromInt(i) },
                    .uint => |u| .{ .double_v = @floatFromInt(u) },
                    else => return error.Parse,
                };
            } else if (std.mem.eql(u8, field, "string_value")) {
                _ = try p.next();
                out = .{ .string_v = try p.readString() };
            } else if (std.mem.eql(u8, field, "bytes_value")) {
                _ = try p.next();
                out = .{ .bytes_v = try p.readString() };
            } else if (std.mem.eql(u8, field, "type_value")) {
                _ = try p.next();
                out = .{ .type_v = try p.readString() };
            } else if (std.mem.eql(u8, field, "list_value")) {
                out = .{ .list_v = try p.parseListValue() };
            } else if (std.mem.eql(u8, field, "map_value")) {
                out = .{ .map_v = try p.parseMapValue() };
            } else if (std.mem.eql(u8, field, "object_value")) {
                out = try p.parseObjectValue();
            } else if (std.mem.eql(u8, field, "enum_value")) {
                out = .enum_v;
                p.skipped_unsupported = true;
                try p.skipFieldValue();
            } else {
                try p.skipFieldValue();
            }
        }
        return out;
    }

    fn parseListValue(p: *Parser) Error![]const TValue {
        const lead = try p.lex.peek();
        if (lead == .colon) _ = try p.next();
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        var items = std.ArrayList(TValue){};
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "values")) {
                try items.append(p.alloc, try p.parseValueMessage());
            } else {
                try p.skipFieldValue();
            }
        }
        return items.toOwnedSlice(p.alloc);
    }

    fn parseMapValue(p: *Parser) Error![]const TEntry {
        const lead = try p.lex.peek();
        if (lead == .colon) _ = try p.next();
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        var entries = std.ArrayList(TEntry){};
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "entries")) {
                const elead = try p.lex.peek();
                if (elead == .colon) _ = try p.next();
                const eopen = try p.next();
                if (eopen != .lbrace) return error.Parse;
                var key: TValue = .unset;
                var value: TValue = .unset;
                while (true) {
                    const et = try p.next();
                    if (et == .rbrace) break;
                    if (et != .ident) return error.Parse;
                    if (std.mem.eql(u8, et.ident, "key")) {
                        key = try p.parseValueMessage();
                    } else if (std.mem.eql(u8, et.ident, "value")) {
                        value = try p.parseValueMessage();
                    } else {
                        try p.skipFieldValue();
                    }
                }
                try entries.append(p.alloc, .{ .key = key, .value = value });
            } else {
                try p.skipFieldValue();
            }
        }
        return entries.toOwnedSlice(p.alloc);
    }

    fn parseObjectValue(p: *Parser) Error!TValue {
        const lead = try p.lex.peek();
        if (lead == .colon) _ = try p.next();
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        // Expect [type.googleapis.com/<type>] { fields... }
        const url = try p.next();
        if (url != .url) return error.Parse;
        const type_name = url.url;
        const body_open = try p.next();
        if (body_open != .lbrace) return error.Parse;
        var seconds: i64 = 0;
        var nanos: i64 = 0;
        var saw_field = false;
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "seconds")) {
                _ = try p.next();
                seconds = (try p.next()).int;
                saw_field = true;
            } else if (std.mem.eql(u8, t.ident, "nanos")) {
                _ = try p.next();
                nanos = (try p.next()).int;
                saw_field = true;
            } else {
                try p.skipFieldValue();
                p.skipped_unsupported = true;
            }
        }
        // Consume the object_value message's own closing brace (the Any
        // body's brace was consumed by the loop above).
        const close = try p.next();
        if (close != .rbrace) return error.Parse;

        if (std.mem.eql(u8, type_name, "type.googleapis.com/google.protobuf.Duration")) {
            return .{ .obj_duration = .{ .seconds = seconds, .nanos = @intCast(nanos) } };
        }
        if (std.mem.eql(u8, type_name, "type.googleapis.com/google.protobuf.Timestamp")) {
            return .{ .obj_timestamp = .{ .seconds = seconds, .nanos = @intCast(nanos) } };
        }
        p.skipped_unsupported = true;
        return .{ .obj_other = type_name };
    }

    fn parseErrorSet(p: *Parser) Error![]const []const u8 {
        const lead = try p.lex.peek();
        if (lead == .colon) _ = try p.next();
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        var msgs = std.ArrayList([]const u8){};
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "errors")) {
                const elead = try p.lex.peek();
                if (elead == .colon) _ = try p.next();
                const eopen = try p.next();
                if (eopen != .lbrace) return error.Parse;
                while (true) {
                    const et = try p.next();
                    if (et == .rbrace) break;
                    if (et != .ident) return error.Parse;
                    if (std.mem.eql(u8, et.ident, "message")) {
                        _ = try p.next();
                        try msgs.append(p.alloc, try p.readString());
                    } else {
                        try p.skipFieldValue();
                    }
                }
            } else {
                try p.skipFieldValue();
            }
        }
        return msgs.toOwnedSlice(p.alloc);
    }

    fn parseAnyErrorSet(p: *Parser) Error![]const []const u8 {
        // any_eval_errors { errors { errors { message } } } -- take union of
        // all messages.
        const open = try p.next();
        if (open != .lbrace) return error.Parse;
        var msgs = std.ArrayList([]const u8){};
        while (true) {
            try p.skipCommas();
            const t = try p.next();
            if (t == .rbrace) break;
            if (t != .ident) return error.Parse;
            if (std.mem.eql(u8, t.ident, "errors")) {
                const inner = try p.parseErrorSet();
                try msgs.appendSlice(p.alloc, inner);
            } else {
                try p.skipFieldValue();
            }
        }
        return msgs.toOwnedSlice(p.alloc);
    }

    /// Skip a field value of unknown shape: `: scalar` or `{ ... }`.
    fn skipFieldValue(p: *Parser) Error!void {
        const t = try p.next();
        switch (t) {
            .colon => {
                const v = try p.next();
                switch (v) {
                    .lbrace, .langle => try p.skipMessageBody(v == .langle),
                    else => {},
                }
            },
            .lbrace, .langle => try p.skipMessageBody(t == .langle),
            else => {},
        }
    }

    fn skipMessageBody(p: *Parser, angle: bool) Error!void {
        var depth: usize = 1;
        while (depth > 0) {
            const t = try p.next();
            switch (t) {
                .lbrace => depth += 1,
                .langle => depth += 1,
                .rbrace => depth -= 1,
                .rangle => depth -= 1,
                .eof => return error.Parse,
                else => {},
            }
        }
        _ = angle;
    }
};

test "parse a small test file" {
    const src =
        \\name: "basic"
        \\section {
        \\  name: "s1"
        \\  test {
        \\    name: "t1"
        \\    expr: "1 + 1"
        \\    value { int64_value: 2 }
        \\  }
        \\  test {
        \\    name: "t2"
        \\    expr: "1 / 0"
        \\    eval_error { errors { message: "divide by zero" } }
        \\  }
        \\}
    ;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const f = try Parser.parse(arena.allocator(), src);
    try std.testing.expectEqualStrings("basic", f.name);
    try std.testing.expectEqual(@as(usize, 1), f.sections.len);
    const s = f.sections[0];
    try std.testing.expectEqualStrings("s1", s.name);
    try std.testing.expectEqual(@as(usize, 2), s.tests.len);
    try std.testing.expectEqualStrings("1 + 1", s.tests[0].expr);
    try std.testing.expectEqual(@as(i64, 2), s.tests[0].result.value.int_v);
    try std.testing.expectEqualStrings("divide by zero", s.tests[1].result.eval_error[0]);
}

test "parse object_value duration" {
    const src =
        \\value {
        \\  object_value {
        \\    [type.googleapis.com/google.protobuf.Duration] {
        \\      seconds: 315576000000
        \\      nanos: 999999999
        \\    }
        \\  }
        \\}
    ;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var p = Parser{ .lex = .{ .src = src, .alloc = arena.allocator() }, .alloc = arena.allocator() };
    _ = try p.next(); // "value"
    const v = try p.parseValueMessage();
    try std.testing.expectEqual(@as(i64, 315576000000), v.obj_duration.seconds);
    try std.testing.expectEqual(@as(i32, 999999999), v.obj_duration.nanos);
}

test "parse bindings and escapes" {
    const src =
        \\bindings { key: "b" value { value { bytes_value: "\303\277" } } }
    ;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var p = Parser{ .lex = .{ .src = src, .alloc = arena.allocator() }, .alloc = arena.allocator() };
    _ = try p.next(); // "bindings"
    const b = try p.parseBinding();
    try std.testing.expectEqualStrings("b", b.name);
    try std.testing.expectEqualStrings("\xc3\xbf", b.value.val.bytes_v);
}
