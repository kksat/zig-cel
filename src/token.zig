//! CEL token definitions.
//!
//! Tokens are produced by the lexer (lexer.zig) and consumed by the parser.
//! Each token records its source span for diagnostics. Numeric literals are
//! pre-parsed (with range checks) so the parser can build constant nodes
//! directly; string/bytes literals are cooked by the lexer (escapes resolved,
//! UTF-8 encoded for strings), yielding zero-copy slices when no escape
//! processing is required.

const std = @import("std");

pub const TokenKind = enum(u8) {
    // Literals & identifiers.
    ident,
    int_lit,
    uint_lit,
    double_lit,
    string_lit,
    bytes_lit,
    // Keywords.
    true_kw,
    false_kw,
    null_kw,
    in_kw,
    // Punctuation.
    dot,
    comma,
    lparen,
    rparen,
    lbracket,
    rbracket,
    lbrace,
    rbrace,
    question,
    colon,
    // Operators.
    or_op,
    and_op,
    not,
    lt,
    le,
    gt,
    ge,
    eq,
    ne,
    plus,
    minus,
    star,
    slash,
    percent,
    // Sentinel.
    eof,

    pub fn describe(kind: TokenKind) []const u8 {
        return switch (kind) {
            .ident => "identifier",
            .int_lit => "int literal",
            .uint_lit => "uint literal",
            .double_lit => "double literal",
            .string_lit => "string literal",
            .bytes_lit => "bytes literal",
            .true_kw => "'true'",
            .false_kw => "'false'",
            .null_kw => "'null'",
            .in_kw => "'in'",
            .dot => "'.'",
            .comma => "','",
            .lparen => "'('",
            .rparen => "')'",
            .lbracket => "'['",
            .rbracket => "']'",
            .lbrace => "'{'",
            .rbrace => "'}'",
            .question => "'?'",
            .colon => "':'",
            .or_op => "'||'",
            .and_op => "'&&'",
            .not => "'!'",
            .lt => "'<'",
            .le => "'<='",
            .gt => "'>'",
            .ge => "'>='",
            .eq => "'=='",
            .ne => "'!='",
            .plus => "'+'",
            .minus => "'-'",
            .star => "'*'",
            .slash => "'/'",
            .percent => "'%'",
            .eof => "end of input",
        };
    }
};

/// Words that are lexical keywords (literals + `in`); they cannot be used as
/// identifiers, function names, or selectors.
pub const keywords = [_][]const u8{ "true", "false", "null", "in" };

/// Reserved words: cannot be used as identifiers or function names, but are
/// permitted in receiver-call position (`a.package()`) and as message/struct
/// name segments. The lexer reports them as `ident` with a reserved flag so
/// the parser can enforce the context.
pub const reserved = [_][]const u8{
    "as",        "break",  "const",  "continue", "else",  "for",
    "function",  "if",     "import", "let",      "loop",  "package",
    "namespace", "return", "var",    "void",     "while",
};

pub fn isReserved(word: []const u8) bool {
    for (reserved) |r| {
        if (std.mem.eql(u8, r, word)) return true;
    }
    return false;
}

pub const Number = union(enum) {
    int: i64,
    uint: u64,
    double: f64,
};

pub const Token = struct {
    kind: TokenKind,
    /// Byte offset of the first character of the token.
    start: u32,
    /// Byte offset one past the last character of the token.
    end: u32,
    /// For ident: the identifier text (source slice).
    /// For numbers: the literal text (source slice).
    text: []const u8 = &.{},
    /// Cooked value for string/bytes literals; source slice when the literal
    /// required no escape processing (zero-copy fast path).
    str: []const u8 = &.{},
    /// Parsed numeric value for numeric literals.
    num: Number = .{ .int = 0 },
    /// True for int literals whose magnitude exceeds maxInt(i64) but fits in
    /// u64; only valid as the operand of unary minus (min-int literal).
    overflow: bool = false,
    /// True when `text` is a reserved word (see `reserved`).
    is_reserved: bool = false,
};
