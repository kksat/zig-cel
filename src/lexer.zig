//! CEL lexer.
//!
//! Produces the token stream defined by the CEL lexis (see
//! cel-spec/doc/langdef.md): identifiers, reserved words, int/uint/double
//! literals (decimal and hex), and string/bytes literals in all four quoting
//! styles (single, double, triple-single, triple-double) with optional raw
//! (`r`/`R`) and bytes (`b`/`B`) prefixes. Escape sequences are resolved by
//! the lexer; when a literal contains no escapes, the cooked value is a
//! zero-copy slice into the source text.

const std = @import("std");
const token = @import("token.zig");

pub const Token = token.Token;
pub const TokenKind = token.TokenKind;

pub const Error = error{ Lex, OutOfMemory };

pub const Lexer = struct {
    src: []const u8,
    pos: usize = 0,
    alloc: std.mem.Allocator,
    err_msg: []const u8 = "",
    err_pos: usize = 0,

    const self = Lexer;

    pub fn init(alloc: std.mem.Allocator, src: []const u8) Lexer {
        return .{ .src = src, .alloc = alloc };
    }

    fn fail(lex: *Lexer, pos: usize, msg: []const u8) Error {
        lex.err_msg = msg;
        lex.err_pos = pos;
        return error.Lex;
    }

    fn peekAt(lex: *Lexer, offset: usize) u8 {
        const i = lex.pos + offset;
        return if (i < lex.src.len) lex.src[i] else 0;
    }

    fn skipTrivia(lex: *Lexer) void {
        while (lex.pos < lex.src.len) {
            const c = lex.src[lex.pos];
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == 0x0c) {
                lex.pos += 1;
            } else if (c == '/' and lex.peekAt(1) == '/') {
                lex.pos += 2;
                while (lex.pos < lex.src.len and lex.src[lex.pos] != '\n') lex.pos += 1;
            } else {
                break;
            }
        }
    }

    /// Lex the next token. Returns a token with kind `.eof` at end of input.
    pub fn next(lex: *Lexer) Error!Token {
        lex.skipTrivia();
        const start = lex.pos;
        if (lex.pos >= lex.src.len) return .{ .kind = .eof, .start = @intCast(start), .end = @intCast(start) };
        const c = lex.src[lex.pos];
        switch (c) {
            'a'...'z', 'A'...'Z', '_' => return lex.lexIdentOrPrefixedString(),
            '0'...'9' => return lex.lexNumber(),
            '.' => {
                if (lex.peekAt(1) >= '0' and lex.peekAt(1) <= '9') return lex.lexNumber();
                lex.pos += 1;
                return lex.punct(.dot, start);
            },
            '"' => return lex.lexString(.{ .is_bytes = false, .raw = false, .start = start }),
            '\'' => return lex.lexString(.{ .is_bytes = false, .raw = false, .start = start }),
            '(' => {
                lex.pos += 1;
                return lex.punct(.lparen, start);
            },
            ')' => {
                lex.pos += 1;
                return lex.punct(.rparen, start);
            },
            '[' => {
                lex.pos += 1;
                return lex.punct(.lbracket, start);
            },
            ']' => {
                lex.pos += 1;
                return lex.punct(.rbracket, start);
            },
            '{' => {
                lex.pos += 1;
                return lex.punct(.lbrace, start);
            },
            '}' => {
                lex.pos += 1;
                return lex.punct(.rbrace, start);
            },
            ',' => {
                lex.pos += 1;
                return lex.punct(.comma, start);
            },
            '?' => {
                lex.pos += 1;
                return lex.punct(.question, start);
            },
            ':' => {
                lex.pos += 1;
                return lex.punct(.colon, start);
            },
            '+' => {
                lex.pos += 1;
                return lex.punct(.plus, start);
            },
            '-' => {
                lex.pos += 1;
                return lex.punct(.minus, start);
            },
            '*' => {
                lex.pos += 1;
                return lex.punct(.star, start);
            },
            '/' => {
                lex.pos += 1;
                return lex.punct(.slash, start);
            },
            '%' => {
                lex.pos += 1;
                return lex.punct(.percent, start);
            },
            '|' => {
                if (lex.peekAt(1) == '|') {
                    lex.pos += 2;
                    return lex.punct(.or_op, start);
                }
                return lex.fail(start, "unexpected character '|'");
            },
            '&' => {
                if (lex.peekAt(1) == '&') {
                    lex.pos += 2;
                    return lex.punct(.and_op, start);
                }
                return lex.fail(start, "unexpected character '&'");
            },
            '!' => {
                if (lex.peekAt(1) == '=') {
                    lex.pos += 2;
                    return lex.punct(.ne, start);
                }
                lex.pos += 1;
                return lex.punct(.not, start);
            },
            '<' => {
                if (lex.peekAt(1) == '=') {
                    lex.pos += 2;
                    return lex.punct(.le, start);
                }
                lex.pos += 1;
                return lex.punct(.lt, start);
            },
            '>' => {
                if (lex.peekAt(1) == '=') {
                    lex.pos += 2;
                    return lex.punct(.ge, start);
                }
                lex.pos += 1;
                return lex.punct(.gt, start);
            },
            '=' => {
                if (lex.peekAt(1) == '=') {
                    lex.pos += 2;
                    return lex.punct(.eq, start);
                }
                return lex.fail(start, "unexpected character '='");
            },
            else => return lex.fail(start, "unexpected character"),
        }
    }

    fn punct(lex: *Lexer, kind: TokenKind, start: usize) Token {
        return .{ .kind = kind, .start = @intCast(start), .end = @intCast(lex.pos) };
    }

    fn isIdentStart(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isIdentChar(c: u8) bool {
        return isIdentStart(c) or (c >= '0' and c <= '9');
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isHexDigit(c: u8) bool {
        return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
    }

    /// Lex an identifier, keyword, or a string/bytes literal prefixed with
    /// raw/bytes markers (`r"..."`, `b'...'`, `br"..."`, ...).
    fn lexIdentOrPrefixedString(lex: *Lexer) Error!Token {
        const start = lex.pos;
        while (lex.pos < lex.src.len and isIdentChar(lex.src[lex.pos])) lex.pos += 1;
        const word = lex.src[start..lex.pos];
        const q = lex.peekAt(0);

        // A word is a string prefix if it consists only of 'r'/'R'/'b'/'B'
        // characters and is followed by a quote.
        if (q == '"' or q == '\'') {
            var raw = false;
            var is_bytes = false;
            var ok = word.len > 0 and word.len <= 2;
            for (word) |ch| {
                switch (ch) {
                    'r', 'R' => raw = true,
                    'b', 'B' => is_bytes = true,
                    else => ok = false,
                }
            }
            if (ok) return lex.lexString(.{ .is_bytes = is_bytes, .raw = raw, .start = start });
        }

        if (std.mem.eql(u8, word, "true")) return lex.punct(.true_kw, start);
        if (std.mem.eql(u8, word, "false")) return lex.punct(.false_kw, start);
        if (std.mem.eql(u8, word, "null")) return lex.punct(.null_kw, start);
        if (std.mem.eql(u8, word, "in")) return lex.punct(.in_kw, start);
        return .{
            .kind = .ident,
            .start = @intCast(start),
            .end = @intCast(lex.pos),
            .text = word,
            .is_reserved = token.isReserved(word),
        };
    }

    fn lexNumber(lex: *Lexer) Error!Token {
        const start = lex.pos;

        // Hex literal.
        if (lex.src[lex.pos] == '0' and (lex.peekAt(1) == 'x' or lex.peekAt(1) == 'X')) {
            lex.pos += 2;
            const digits_start = lex.pos;
            while (lex.pos < lex.src.len and isHexDigit(lex.src[lex.pos])) lex.pos += 1;
            if (lex.pos == digits_start) return lex.fail(start, "invalid hex literal: expected digit after 0x");
            const digits = lex.src[digits_start..lex.pos];
            if (lex.pos < lex.src.len and (lex.src[lex.pos] == 'u' or lex.src[lex.pos] == 'U')) {
                lex.pos += 1;
                const value = std.fmt.parseUnsigned(u64, digits, 16) catch
                    return lex.fail(start, "uint literal out of range");
                return .{ .kind = .uint_lit, .start = @intCast(start), .end = @intCast(lex.pos), .text = lex.src[start..lex.pos], .num = .{ .uint = value } };
            }
            // A hex literal followed by more identifier characters is invalid.
            if (lex.pos < lex.src.len and isIdentChar(lex.src[lex.pos]))
                return lex.fail(start, "invalid hex literal");
            const value = std.fmt.parseInt(i64, digits, 16) catch {
                const big = std.fmt.parseUnsigned(u64, digits, 16) catch
                    return lex.fail(start, "int literal out of range");
                return .{ .kind = .int_lit, .start = @intCast(start), .end = @intCast(lex.pos), .text = lex.src[start..lex.pos], .num = .{ .uint = big }, .overflow = true };
            };
            return .{ .kind = .int_lit, .start = @intCast(start), .end = @intCast(lex.pos), .text = lex.src[start..lex.pos], .num = .{ .int = value } };
        }

        // Decimal digits.
        const int_digits_start = lex.pos;
        while (lex.pos < lex.src.len and isDigit(lex.src[lex.pos])) lex.pos += 1;
        const int_digits = lex.src[int_digits_start..lex.pos];
        if (int_digits.len > 1 and int_digits[0] == '0')
            return lex.fail(start, "invalid literal: leading zeros are not allowed");

        var is_double = false;
        var have_dot = false;
        var frac_digits: []const u8 = &.{};

        // Fraction: '.' must be followed by a digit.
        if (lex.pos < lex.src.len and lex.src[lex.pos] == '.' and isDigit(lex.peekAt(1))) {
            is_double = true;
            have_dot = true;
            lex.pos += 1;
            const frac_start = lex.pos;
            while (lex.pos < lex.src.len and isDigit(lex.src[lex.pos])) lex.pos += 1;
            frac_digits = lex.src[frac_start..lex.pos];
        }

        // Exponent.
        if (lex.pos < lex.src.len and (lex.src[lex.pos] == 'e' or lex.src[lex.pos] == 'E')) {
            var exp_end = lex.pos + 1;
            if (exp_end < lex.src.len and (lex.src[exp_end] == '+' or lex.src[exp_end] == '-')) exp_end += 1;
            if (exp_end < lex.src.len and isDigit(lex.src[exp_end])) {
                is_double = true;
                lex.pos = exp_end;
                while (lex.pos < lex.src.len and isDigit(lex.src[lex.pos])) lex.pos += 1;
            }
        }

        if (is_double) {
            const value = std.fmt.parseFloat(f64, lex.src[start..lex.pos]) catch
                return lex.fail(start, "invalid double literal");
            return .{ .kind = .double_lit, .start = @intCast(start), .end = @intCast(lex.pos), .text = lex.src[start..lex.pos], .num = .{ .double = value } };
        }

        if (have_dot) return lex.fail(start, "invalid double literal: expected digit after '.'");

        // Integer or uint.
        if (lex.pos < lex.src.len and (lex.src[lex.pos] == 'u' or lex.src[lex.pos] == 'U')) {
            lex.pos += 1;
            const value = std.fmt.parseUnsigned(u64, int_digits, 10) catch
                return lex.fail(start, "uint literal out of range");
            return .{ .kind = .uint_lit, .start = @intCast(start), .end = @intCast(lex.pos), .text = lex.src[start..lex.pos], .num = .{ .uint = value } };
        }
        // An integer literal followed by identifier characters (e.g. `12x`)
        // is invalid; this also rejects suffixes like 'f'.
        if (lex.pos < lex.src.len and isIdentChar(lex.src[lex.pos]))
            return lex.fail(start, "invalid literal");
        const value = std.fmt.parseInt(i64, int_digits, 10) catch {
            const big = std.fmt.parseUnsigned(u64, int_digits, 10) catch
                return lex.fail(start, "int literal out of range");
            return .{ .kind = .int_lit, .start = @intCast(start), .end = @intCast(lex.pos), .text = lex.src[start..lex.pos], .num = .{ .uint = big }, .overflow = true };
        };
        return .{ .kind = .int_lit, .start = @intCast(start), .end = @intCast(lex.pos), .text = lex.src[start..lex.pos], .num = .{ .int = value } };
    }

    const StringPrefix = struct {
        is_bytes: bool,
        raw: bool,
        start: usize,
    };

    fn lexString(lex: *Lexer, prefix: StringPrefix) Error!Token {
        const start = prefix.start;
        const quote = lex.src[lex.pos];
        // Detect triple-quoted form.
        var delim_len: usize = 1;
        if (lex.peekAt(1) == quote and lex.peekAt(2) == quote) delim_len = 3;
        lex.pos += delim_len;
        const content_start = lex.pos;

        // Fast path: scan for the closing delimiter, checking for escapes.
        while (lex.pos < lex.src.len) {
            const c = lex.src[lex.pos];
            if (c == '\\') {
                if (prefix.raw) {
                    lex.pos += 1; // raw: backslash is literal
                    continue;
                }
                // Escape sequences require cooking; restart content scan.
                return lex.lexStringSlow(prefix, quote, delim_len, content_start, start);
            }
            if (c == quote) {
                if (delim_len == 1) {
                    const content = lex.src[content_start..lex.pos];
                    lex.pos += 1;
                    return lex.finishString(prefix, content, content_start, start, quote, delim_len);
                }
                // Check for triple delimiter.
                if (lex.peekAt(1) == quote and lex.peekAt(2) == quote) {
                    const content = lex.src[content_start..lex.pos];
                    lex.pos += 3;
                    return lex.finishString(prefix, content, content_start, start, quote, delim_len);
                }
                lex.pos += 1;
                continue;
            }
            if (delim_len == 1 and (c == '\n' or c == '\r'))
                return lex.fail(lex.pos, "newline not allowed in single-quoted string literal");
            lex.pos += 1;
        }
        return lex.fail(start, "unterminated string literal");
    }

    /// Common validation for the zero-copy fast path.
    fn finishString(lex: *Lexer, prefix: StringPrefix, content: []const u8, content_start: usize, start: usize, quote: u8, delim_len: usize) Error!Token {
        _ = quote;
        _ = delim_len;
        _ = content_start;
        if (!prefix.is_bytes and !std.unicode.utf8ValidateSlice(content))
            return lex.fail(start, "string literal is not valid UTF-8");
        const kind: TokenKind = if (prefix.is_bytes) .bytes_lit else .string_lit;
        return .{
            .kind = kind,
            .start = @intCast(start),
            .end = @intCast(lex.pos),
            .text = lex.src[start..lex.pos],
            .str = content,
        };
    }

    /// Slow path: the literal contains at least one escape sequence; build the
    /// cooked value into an arena buffer.
    fn lexStringSlow(lex: *Lexer, prefix: StringPrefix, quote: u8, delim_len: usize, content_start: usize, start: usize) Error!Token {
        var buf: std.ArrayList(u8) = .empty;
        errdefer buf.deinit(lex.alloc);
        try buf.ensureTotalCapacity(lex.alloc, @intCast(lex.pos - content_start));

        lex.pos = content_start;
        while (lex.pos < lex.src.len) {
            const c = lex.src[lex.pos];
            if (c == '\\') {
                lex.pos += 1;
                try lex.appendEscape(prefix.is_bytes, &buf);
                continue;
            }
            if (c == quote) {
                if (delim_len == 1) {
                    lex.pos += 1;
                    return lex.cookedString(prefix, &buf, start);
                }
                if (lex.peekAt(1) == quote and lex.peekAt(2) == quote) {
                    lex.pos += 3;
                    return lex.cookedString(prefix, &buf, start);
                }
                try buf.append(lex.alloc, c);
                lex.pos += 1;
                continue;
            }
            if (delim_len == 1 and (c == '\n' or c == '\r'))
                return lex.fail(lex.pos, "newline not allowed in single-quoted string literal");
            try buf.append(lex.alloc, c);
            lex.pos += 1;
        }
        return lex.fail(start, "unterminated string literal");
    }

    fn cookedString(lex: *Lexer, prefix: StringPrefix, buf: *std.ArrayList(u8), start: usize) Error!Token {
        const content = try buf.toOwnedSlice(lex.alloc);
        if (!prefix.is_bytes and !std.unicode.utf8ValidateSlice(content))
            return lex.fail(start, "string literal is not valid UTF-8");
        const kind: TokenKind = if (prefix.is_bytes) .bytes_lit else .string_lit;
        return .{
            .kind = kind,
            .start = @intCast(start),
            .end = @intCast(lex.pos),
            .text = lex.src[start..lex.pos],
            .str = content,
        };
    }

    fn hexValue(c: u8) ?u8 {
        return switch (c) {
            '0'...'9' => c - '0',
            'a'...'f' => c - 'a' + 10,
            'A'...'F' => c - 'A' + 10,
            else => null,
        };
    }

    fn readHexDigits(lex: *Lexer, count: usize, value: *u32) bool {
        var v: u32 = 0;
        var i: usize = 0;
        while (i < count) : (i += 1) {
            const h = hexValue(lex.peekAt(i)) orelse return false;
            v = v * 16 + h;
        }
        lex.pos += count;
        value.* = v;
        return true;
    }

    fn appendEscape(lex: *Lexer, is_bytes: bool, buf: *std.ArrayList(u8)) Error!void {
        const esc_pos = lex.pos - 1;
        if (lex.pos >= lex.src.len) return lex.fail(esc_pos, "unterminated escape sequence");
        const c = lex.src[lex.pos];
        switch (c) {
            'a' => {
                try buf.append(lex.alloc, 0x07);
                lex.pos += 1;
            },
            'b' => {
                try buf.append(lex.alloc, 0x08);
                lex.pos += 1;
            },
            'f' => {
                try buf.append(lex.alloc, 0x0c);
                lex.pos += 1;
            },
            'n' => {
                try buf.append(lex.alloc, '\n');
                lex.pos += 1;
            },
            'r' => {
                try buf.append(lex.alloc, '\r');
                lex.pos += 1;
            },
            't' => {
                try buf.append(lex.alloc, '\t');
                lex.pos += 1;
            },
            'v' => {
                try buf.append(lex.alloc, 0x0b);
                lex.pos += 1;
            },
            '\\', '?', '"', '\'', '`' => {
                try buf.append(lex.alloc, c);
                lex.pos += 1;
            },
            'x', 'X' => {
                lex.pos += 1;
                var v: u32 = 0;
                if (!lex.readHexDigits(2, &v)) return lex.fail(esc_pos, "invalid hex escape: expected two hex digits");
                if (is_bytes) {
                    try buf.append(lex.alloc, @intCast(v));
                } else {
                    try lex.appendCodePoint(v, esc_pos, buf);
                }
            },
            'u' => {
                if (is_bytes) return lex.fail(esc_pos, "unicode escape not supported in bytes literal");
                lex.pos += 1;
                var v: u32 = 0;
                if (!lex.readHexDigits(4, &v)) return lex.fail(esc_pos, "invalid unicode escape: expected four hex digits");
                try lex.appendCodePoint(v, esc_pos, buf);
            },
            'U' => {
                if (is_bytes) return lex.fail(esc_pos, "unicode escape not supported in bytes literal");
                lex.pos += 1;
                var v: u32 = 0;
                if (!lex.readHexDigits(8, &v)) return lex.fail(esc_pos, "invalid unicode escape: expected eight hex digits");
                try lex.appendCodePoint(v, esc_pos, buf);
            },
            '0'...'3' => {
                // Octal escape: three octal digits, value 0-255.
                var v: u32 = 0;
                var i: usize = 0;
                while (i < 3) : (i += 1) {
                    const d = lex.peekAt(i);
                    if (d < '0' or d > '7') return lex.fail(esc_pos, "invalid octal escape: expected three octal digits");
                    v = v * 8 + (d - '0');
                }
                lex.pos += 3;
                if (is_bytes) {
                    try buf.append(lex.alloc, @intCast(v));
                } else {
                    try lex.appendCodePoint(v, esc_pos, buf);
                }
            },
            else => return lex.fail(esc_pos, "invalid escape sequence"),
        }
    }

    fn appendCodePoint(lex: *Lexer, v: u32, esc_pos: usize, buf: *std.ArrayList(u8)) Error!void {
        if (v > 0x10FFFF) return lex.fail(esc_pos, "invalid code point in escape");
        if (v >= 0xD800 and v <= 0xDFFF) return lex.fail(esc_pos, "surrogate code point not allowed in escape");
        var tmp: [4]u8 = undefined;
        const n = std.unicode.utf8Encode(@intCast(v), &tmp) catch
            return lex.fail(esc_pos, "invalid code point in escape");
        try buf.appendSlice(lex.alloc, tmp[0..n]);
    }
};

test "lex simple expression" {
    var lex = Lexer.init(std.testing.allocator, "a.b <= 42 && 'x' in [1, 2.5]");
    var toks: std.ArrayList(Token) = .empty;
    defer toks.deinit(std.testing.allocator);
    while (true) {
        const t = try lex.next();
        if (t.kind == .eof) break;
        try toks.append(std.testing.allocator, t);
    }
    const kinds = [_]TokenKind{ .ident, .dot, .ident, .le, .int_lit, .and_op, .string_lit, .in_kw, .lbracket, .int_lit, .comma, .double_lit, .rbracket };
    try std.testing.expectEqual(kinds.len, toks.items.len);
    for (kinds, toks.items) |k, t| try std.testing.expectEqual(k, t.kind);
    try std.testing.expectEqualStrings("x", toks.items[6].str);
    try std.testing.expectEqual(@as(i64, 42), toks.items[4].num.int);
    try std.testing.expectEqual(@as(f64, 2.5), toks.items[11].num.double);
}

test "lex strings" {
    const cases = .{
        .{ .src = "\"hello\"", .want = "hello" },
        .{ .src = "'a\\nb'", .want = "a\nb" },
        .{ .src = "r'a\\nb'", .want = "a\\nb" },
        .{ .src = "\"\\x41\"", .want = "A" },
        .{ .src = "\"\\101\"", .want = "A" },
        .{ .src = "\"\\u00e9\"", .want = "é" },
        .{ .src = "\"\\U0001F92A\"", .want = "🤪" },
        .{ .src = "'''triple'''", .want = "triple" },
        .{ .src = "\"\"\"a\nb\"\"\"", .want = "a\nb" },
        .{ .src = "b'\\xff'", .want_bytes = true },
    };
    inline for (cases) |c| {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();
        var lex = Lexer.init(arena.allocator(), c.src);
        const t = try lex.next();
        const e = try lex.next();
        try std.testing.expectEqual(TokenKind.eof, e.kind);
        if (@hasField(@TypeOf(c), "want_bytes")) {
            try std.testing.expectEqual(TokenKind.bytes_lit, t.kind);
        } else {
            try std.testing.expectEqual(TokenKind.string_lit, t.kind);
            try std.testing.expectEqualStrings(c.want, t.str);
        }
    }
}

test "lex string errors" {
    const cases = [_][]const u8{
        "\"unterminated",
        "'newline\nnot allowed'",
        "\"\\q\"",
        "\"\\u12\"",
        "\"\\uD800\"",
        "\"\\U00110000\"",
        "b'\\u00e9'",
        "01",
        "18446744073709551616u",
        "18446744073709551616",
        "0x",
        "12abc",
        "@",
        "&",
    };
    for (cases) |src| {
        var lex = Lexer.init(std.testing.allocator, src);
        try std.testing.expectError(error.Lex, lex.next());
    }
}

test "lex numbers" {
    const cases = .{
        .{ .src = "0", .k = TokenKind.int_lit, .i = 0 },
        .{ .src = "42", .k = TokenKind.int_lit, .i = 42 },
        .{ .src = "9223372036854775807", .k = TokenKind.int_lit, .i = 9223372036854775807 },
        .{ .src = "0u", .k = TokenKind.uint_lit, .u = 0 },
        .{ .src = "18446744073709551615U", .k = TokenKind.uint_lit, .u = 18446744073709551615 },
        .{ .src = "0xFF", .k = TokenKind.int_lit, .i = 255 },
        .{ .src = "0Xffu", .k = TokenKind.uint_lit, .u = 255 },
        .{ .src = "3.14", .k = TokenKind.double_lit, .d = 3.14 },
        .{ .src = ".5", .k = TokenKind.double_lit, .d = 0.5 },
        .{ .src = "1e3", .k = TokenKind.double_lit, .d = 1000.0 },
        .{ .src = "2.5e-2", .k = TokenKind.double_lit, .d = 0.025 },
        .{ .src = "1E+2", .k = TokenKind.double_lit, .d = 100.0 },
    };
    inline for (cases) |c| {
        var lex = Lexer.init(std.testing.allocator, c.src);
        const t = try lex.next();
        try std.testing.expectEqual(c.k, t.kind);
        switch (c.k) {
            .int_lit => try std.testing.expectEqual(c.i, t.num.int),
            .uint_lit => try std.testing.expectEqual(c.u, t.num.uint),
            .double_lit => try std.testing.expectEqual(c.d, t.num.double),
            else => unreachable,
        }
    }
}

test "lex min int literal as overflow token" {
    var lex = Lexer.init(std.testing.allocator, "9223372036854775808");
    const t = try lex.next();
    try std.testing.expectEqual(TokenKind.int_lit, t.kind);
    try std.testing.expect(t.overflow);
    try std.testing.expectEqual(@as(u64, 9223372036854775808), t.num.uint);
}

test "lex keywords and reserved" {
    var lex = Lexer.init(std.testing.allocator, "true false null in as while");
    const kw_kinds = [_]TokenKind{ .true_kw, .false_kw, .null_kw, .in_kw, .ident, .ident };
    for (kw_kinds) |k| {
        const t = try lex.next();
        try std.testing.expectEqual(k, t.kind);
    }
    // 'as' and 'while' are reserved identifiers.
    var lex2 = Lexer.init(std.testing.allocator, "as while");
    const t1 = try lex2.next();
    const t2 = try lex2.next();
    try std.testing.expect(t1.is_reserved);
    try std.testing.expect(t2.is_reserved);
}

test "lex comments and whitespace" {
    var lex = Lexer.init(std.testing.allocator, "a // comment\n + \tb");
    const t1 = try lex.next();
    const t2 = try lex.next();
    const t3 = try lex.next();
    try std.testing.expectEqual(TokenKind.ident, t1.kind);
    try std.testing.expectEqualStrings("a", t1.text);
    try std.testing.expectEqual(TokenKind.plus, t2.kind);
    try std.testing.expectEqual(TokenKind.ident, t3.kind);
    try std.testing.expectEqualStrings("b", t3.text);
}

test "lex block comment is not a comment" {
    var lex = Lexer.init(std.testing.allocator, "/* not a comment */");
    try std.testing.expectEqual(TokenKind.slash, (try lex.next()).kind);
    try std.testing.expectEqual(TokenKind.star, (try lex.next()).kind);
}

test "raw triple string keeps backslashes" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var lex = Lexer.init(arena.allocator(), "r\"\"\"\\d+\"\"\"");
    const t = try lex.next();
    try std.testing.expectEqual(TokenKind.string_lit, t.kind);
    try std.testing.expectEqualStrings("\\d+", t.str);
}
