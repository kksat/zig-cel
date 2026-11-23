//! A small regular-expression engine covering the commonly used RE2 syntax
//! subset: literals (UTF-8), '.', character classes with ranges and
//! negation, escapes (\d \D \w \W \s \S \b \B \n \r \t \f \v \a \xHH),
//! anchors (^ $), groups (capturing and (?:...)), alternation, and
//! quantifiers (* + ? {n} {n,} {n,m}) with or without non-greedy '?'.
//!
//! Matching uses a Thompson NFA simulation (Pike VM without captures), so
//! match time is linear in input length times program size -- untrusted
//! patterns cannot cause exponential backtracking. Only a boolean "matches
//! anywhere" result is required, so quantifier greediness is irrelevant and
//! search semantics are implemented by prepending an any-byte loop to the
//! compiled program (a single simulation pass then covers all start
//! positions; `^`/`$` assertions still see true input boundaries).
//!
//! Byte-oriented: '.' is UTF-8 aware; negated classes match non-ASCII bytes
//! individually (documented limitation for exact character-count patterns).

const std = @import("std");

pub const Error = error{ InvalidPattern, TooComplex, OutOfMemory };

const max_program: usize = 8192; // instruction cap
const max_repeat: u32 = 1000; // {n,m} expansion cap

// ---- AST ----

const Node = union(enum) {
    empty: void,
    /// ASCII bitset; `any_high` also matches bytes >= 0x80.
    char: CharMatch,
    /// Byte range (used for UTF-8 sequence matching).
    range: RangeMatch,
    concat: []Node,
    alt: []Node,
    star: *Node,
    plus: *Node,
    opt: *Node,
    /// {min, max}: min copies then (max-min) optionals; max == null means
    /// unbounded (a trailing star).
    repeat: struct { child: *Node, min: u32, max: ?u32 },
    bol: void,
    eol: void,
    word_boundary: bool,
    /// Matches any UTF-8 character except '\n'.
    any: void,
};

const Bitset = [2]u64; // 128 bits for ASCII

const CharMatch = struct { set: Bitset, any_high: bool };
const RangeMatch = struct { lo: u8, hi: u8 };

fn bitsetHas(set: Bitset, c: u8) bool {
    if (c >= 128) return false;
    return (set[c >> 6] >> @intCast(c & 63)) & 1 != 0;
}

fn bitsetSet(set: *Bitset, c: u8) void {
    if (c < 128) set[c >> 6] |= @as(u64, 1) << @intCast(c & 63);
}

fn bitsetSetRange(set: *Bitset, lo: u8, hi: u8) void {
    var c: u16 = lo;
    while (c <= hi and c < 128) : (c += 1) {
        set[@as(u8, @intCast(c)) >> 6] |= @as(u64, 1) << @intCast(c & 63);
    }
}

// ---- pattern parser (to AST) ----

const PatternParser = struct {
    pat: []const u8,
    pos: usize = 0,
    alloc: std.mem.Allocator,
    depth: usize = 0,

    fn peekByte(p: *PatternParser) ?u8 {
        return if (p.pos < p.pat.len) p.pat[p.pos] else null;
    }

    fn nextByte(p: *PatternParser) ?u8 {
        if (p.pos >= p.pat.len) return null;
        const c = p.pat[p.pos];
        p.pos += 1;
        return c;
    }

    fn newNode(p: *PatternParser, n: Node) Error!*Node {
        // Individual allocations are never moved by the arena (unlike
        // ArrayList buffers, which can be remapped), so interior pointers
        // stay valid.
        const node = try p.alloc.create(Node);
        node.* = n;
        return node;
    }

    fn newSlice(p: *PatternParser, kids: []const Node) Error![]Node {
        const s = try p.alloc.alloc(Node, kids.len);
        @memcpy(s, kids);
        return s;
    }

    /// alternation := concat ('|' concat)*
    fn parseAlternation(p: *PatternParser) Error!*Node {
        p.depth += 1;
        defer p.depth -= 1;
        if (p.depth > 200) return error.TooComplex;
        var branches = std.ArrayList(Node){};
        defer branches.deinit(p.alloc);
        try branches.append(p.alloc, try p.parseConcat());
        while (p.peekByte() == '|') {
            _ = p.nextByte();
            try branches.append(p.alloc, try p.parseConcat());
        }
        if (branches.items.len == 1) {
            const n = branches.items[0];
            // Return a stable pointer: re-append to the node arena.
            return p.newNode(n);
        }
        return p.newNode(.{ .alt = try p.newSlice(branches.items) });
    }

    fn parseConcat(p: *PatternParser) Error!Node {
        var parts = std.ArrayList(Node){};
        defer parts.deinit(p.alloc);
        while (p.peekByte()) |c| {
            if (c == '|' or c == ')') break;
            try parts.append(p.alloc, try p.parseRepeat());
        }
        if (parts.items.len == 0) return .{ .empty = {} };
        if (parts.items.len == 1) return parts.items[0];
        return .{ .concat = try p.newSlice(parts.items) };
    }

    /// repeat := atom quantifier?
    fn parseRepeat(p: *PatternParser) Error!Node {
        const atom = try p.parseAtom();
        const q = p.peekByte() orelse return atom.*;
        var min: u32 = 0;
        var max: ?u32 = null;
        switch (q) {
            '*' => {
                _ = p.nextByte();
            },
            '+' => {
                _ = p.nextByte();
                min = 1;
            },
            '?' => {
                _ = p.nextByte();
                max = 1;
            },
            '{' => {
                const save = p.pos;
                _ = p.nextByte();
                min = p.parseNumber() catch {
                    p.pos = save;
                    return atom.*;
                };
                if (p.peekByte() == ',') {
                    _ = p.nextByte();
                    if (p.peekByte() == '}') {
                        max = null;
                    } else if (p.peekByte() != null) {
                        max = p.parseNumber() catch {
                            p.pos = save;
                            return atom.*;
                        };
                    } else {
                        p.pos = save;
                        return atom.*;
                    }
                } else {
                    max = min;
                }
                if (p.peekByte() != '}') {
                    p.pos = save;
                    return atom.*;
                }
                _ = p.nextByte();
            },
            else => return atom.*,
        }
        // Non-greedy marker: irrelevant for boolean matching.
        if (p.peekByte() == '?') _ = p.nextByte();

        if (min > max_repeat) return error.TooComplex;
        if (max) |mx| {
            if (mx > max_repeat or mx < min) return error.InvalidPattern;
        }
        if (min == 0 and max == null) return .{ .star = atom };
        if (min == 0 and max == 1) return .{ .opt = atom };
        if (min == 1 and max == null) return .{ .plus = atom };
        return .{ .repeat = .{ .child = atom, .min = min, .max = max } };
    }

    fn parseNumber(p: *PatternParser) Error!u32 {
        var v: u64 = 0;
        const start = p.pos;
        while (p.peekByte()) |c| {
            if (c < '0' or c > '9') break;
            v = v * 10 + (c - '0');
            if (v > 100000) return error.TooComplex;
            p.pos += 1;
        }
        if (p.pos == start) return error.InvalidPattern;
        return @intCast(v);
    }

    fn parseAtom(p: *PatternParser) Error!*Node {
        const c = p.nextByte() orelse return error.InvalidPattern;
        switch (c) {
            '(' => {
                if (p.peekByte() == '?') {
                    _ = p.nextByte();
                    const k = p.nextByte() orelse return error.InvalidPattern;
                    // Only non-capturing groups and harmless flags are
                    // supported; flag-scoped groups are rejected.
                    if (k != ':') return error.InvalidPattern;
                }
                const inner = try p.parseAlternation();
                if (p.nextByte() != ')') return error.InvalidPattern;
                return inner;
            },
            '[' => return p.parseClass(),
            '.' => return p.newNode(.{ .any = {} }),
            '^' => return p.newNode(.{ .bol = {} }),
            '$' => return p.newNode(.{ .eol = {} }),
            '\\' => return p.parseEscape(),
            '*', '+', '?' => return error.InvalidPattern,
            else => {
                if (c >= 0x80) {
                    return p.newNode(.{ .range = .{ .lo = c, .hi = c } });
                }
                var set: Bitset = .{ 0, 0 };
                bitsetSet(&set, c);
                return p.newNode(.{ .char = .{ .set = set, .any_high = false } });
            },
        }
    }

    fn shorthandSet(kind: u8) Bitset {
        var set: Bitset = .{ 0, 0 };
        switch (kind) {
            'd' => bitsetSetRange(&set, '0', '9'),
            'w' => {
                bitsetSetRange(&set, 'a', 'z');
                bitsetSetRange(&set, 'A', 'Z');
                bitsetSetRange(&set, '0', '9');
                bitsetSet(&set, '_');
            },
            's' => {
                bitsetSet(&set, ' ');
                bitsetSet(&set, '\t');
                bitsetSet(&set, '\n');
                bitsetSet(&set, '\r');
                bitsetSet(&set, 0x0b);
                bitsetSet(&set, 0x0c);
            },
            else => unreachable,
        }
        return set;
    }

    fn negateSet(set: Bitset) Bitset {
        var out = set;
        out[0] = ~out[0];
        out[1] = ~out[1];
        return out;
    }

    fn parseEscape(p: *PatternParser) Error!*Node {
        const c = p.nextByte() orelse return error.InvalidPattern;
        switch (c) {
            'd', 'D', 'w', 'W', 's', 'S' => {
                var set = shorthandSet(std.ascii.toLower(c));
                if (std.ascii.isUpper(c)) set = negateSet(set);
                // Negated shorthands also match non-ASCII bytes.
                return p.newNode(.{ .char = .{ .set = set, .any_high = std.ascii.isUpper(c) } });
            },
            'n', 'r', 't', 'f', 'v', 'a', '0' => {
                const b: u8 = switch (c) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'f' => 0x0c,
                    'v' => 0x0b,
                    'a' => 0x07,
                    '0' => 0,
                    else => unreachable,
                };
                var set: Bitset = .{ 0, 0 };
                bitsetSet(&set, b);
                return p.newNode(.{ .char = .{ .set = set, .any_high = false } });
            },
            'x' => {
                const h1 = p.hexDigit() orelse return error.InvalidPattern;
                const h2 = p.hexDigit() orelse return error.InvalidPattern;
                var set: Bitset = .{ 0, 0 };
                bitsetSet(&set, h1 * 16 + h2);
                return p.newNode(.{ .char = .{ .set = set, .any_high = false } });
            },
            'b' => return p.newNode(.{ .word_boundary = true }),
            'B' => return p.newNode(.{ .word_boundary = false }),
            'A' => return p.newNode(.{ .bol = {} }),
            'z' => return p.newNode(.{ .eol = {} }),
            else => {
                // Escaped punctuation matches itself.
                var set: Bitset = .{ 0, 0 };
                bitsetSet(&set, c);
                return p.newNode(.{ .char = .{ .set = set, .any_high = false } });
            },
        }
    }

    fn hexDigit(p: *PatternParser) ?u8 {
        const c = p.nextByte() orelse return null;
        return switch (c) {
            '0'...'9' => c - '0',
            'a'...'f' => c - 'a' + 10,
            'A'...'F' => c - 'A' + 10,
            else => null,
        };
    }

    fn parseClass(p: *PatternParser) Error!*Node {
        var set: Bitset = .{ 0, 0 };
        var negate = false;
        var any_high = false;
        if (p.peekByte() == '^') {
            _ = p.nextByte();
            negate = true;
        }
        var first = true;
        while (true) {
            const c = p.nextByte() orelse return error.InvalidPattern;
            if (c == ']' and !first) break;
            first = false;
            var lo: u8 = c;
            if (c == '\\') {
                const e = p.nextByte() orelse return error.InvalidPattern;
                switch (e) {
                    'd', 'D', 'w', 'W', 's', 'S' => {
                        var sh = shorthandSet(std.ascii.toLower(e));
                        if (std.ascii.isUpper(e)) {
                            sh = negateSet(sh);
                            any_high = true;
                        }
                        set[0] |= sh[0];
                        set[1] |= sh[1];
                        continue;
                    },
                    'n', 'r', 't', 'f', 'v', 'a', '0' => {
                        lo = switch (e) {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            'f' => 0x0c,
                            'v' => 0x0b,
                            'a' => 0x07,
                            '0' => 0,
                            else => unreachable,
                        };
                    },
                    'x' => {
                        const h1 = p.hexDigit() orelse return error.InvalidPattern;
                        const h2 = p.hexDigit() orelse return error.InvalidPattern;
                        lo = h1 * 16 + h2;
                    },
                    'b' => lo = 0x08,
                    else => lo = e,
                }
            } else if (c >= 0x80) {
                // Non-ASCII literal byte: include it via any_high? No --
                // classes are byte-exact; add by range.
                set[1] |= @as(u64, 1) << @intCast(c - 64);
                continue;
            }
            // Range?
            if (p.peekByte() == '-' and p.pos + 1 < p.pat.len and p.pat[p.pos + 1] != ']') {
                _ = p.nextByte(); // '-'
                var hi = p.nextByte() orelse return error.InvalidPattern;
                if (hi == '\\') {
                    const e = p.nextByte() orelse return error.InvalidPattern;
                    hi = switch (e) {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        else => e,
                    };
                }
                if (hi < lo) return error.InvalidPattern;
                bitsetSetRange(&set, lo, hi);
                continue;
            }
            bitsetSet(&set, lo);
        }
        if (negate) {
            set = negateSet(set);
            any_high = true;
        }
        return p.newNode(.{ .char = .{ .set = set, .any_high = any_high } });
    }
};

// ---- codegen (AST -> NFA program) ----

const Inst = union(enum) {
    /// Consume one byte matching the bitset (plus any byte >= 0x80 when
    /// `any_high`).
    char: CharMatch,
    /// Consume one byte in [lo, hi].
    range: RangeMatch,
    /// Try continuation `a` first, then `b`.
    split: struct { a: u32, b: u32 },
    jmp: u32,
    /// Assert position 0.
    bol: void,
    /// Assert position == input length.
    eol: void,
    word_boundary: bool,
    match: void,
};

const CodeGen = struct {
    prog: []Inst,
    len: u32 = 0,

    fn emit(g: *CodeGen, inst: Inst) Error!u32 {
        if (g.len >= g.prog.len) return error.TooComplex;
        g.prog[g.len] = inst;
        g.len += 1;
        return g.len - 1;
    }

    fn here(g: *CodeGen) u32 {
        return g.len;
    }

    fn gen(g: *CodeGen, node: *const Node) Error!void {
        switch (node.*) {
            .empty => {},
            .char => |c| _ = try g.emit(.{ .char = c }),
            .range => |r| _ = try g.emit(.{ .range = r }),
            .bol => _ = try g.emit(.{ .bol = {} }),
            .eol => _ = try g.emit(.{ .eol = {} }),
            .word_boundary => |w| _ = try g.emit(.{ .word_boundary = w }),
            .any => {
                // Any UTF-8 character except '\n':
                //   split(ascii, mb)
                // ascii: one char-inst matching all ASCII except '\n'
                // mb: lead [C2-F4], cont [80-BF], then up to two more
                //     optional continuation bytes.
                const s = try g.emit(.{ .split = .{ .a = 0, .b = 0 } });
                g.prog[s].split.a = g.here();
                var ascii: Bitset = .{ ~@as(u64, 0), ~@as(u64, 0) };
                ascii[0] &= ~(@as(u64, 1) << '\n');
                _ = try g.emit(.{ .char = .{ .set = ascii, .any_high = false } });
                const j = try g.emit(.{ .jmp = 0 });
                const mb = g.here();
                _ = try g.emit(.{ .range = .{ .lo = 0xC2, .hi = 0xF4 } }); // lead
                _ = try g.emit(.{ .range = .{ .lo = 0x80, .hi = 0xBF } }); // cont 1
                const s2 = try g.emit(.{ .split = .{ .a = 0, .b = 0 } });
                g.prog[s2].split.a = g.here();
                _ = try g.emit(.{ .range = .{ .lo = 0x80, .hi = 0xBF } }); // cont 2
                const s3 = try g.emit(.{ .split = .{ .a = 0, .b = 0 } });
                g.prog[s3].split.a = g.here();
                _ = try g.emit(.{ .range = .{ .lo = 0x80, .hi = 0xBF } }); // cont 3
                const end = g.here();
                g.prog[s].split.b = mb;
                g.prog[j].jmp = end;
                g.prog[s2].split.b = end;
                g.prog[s3].split.b = end;
            },
            .concat => |kids| {
                for (kids) |*k| try g.gen(k);
            },
            .alt => |branches| {
                // s_i: split(branch_i, s_{i+1}); branch_i; jmp end; ... last
                // branch falls through to end.
                if (branches.len > 128) return error.TooComplex;
                var jmps: [128]u32 = undefined;
                var splits: [128]u32 = undefined;
                var branch_start: [129]u32 = undefined;
                var nj: usize = 0;
                var ns: usize = 0;
                for (branches, 0..) |*b, i| {
                    if (i + 1 < branches.len) {
                        const s = try g.emit(.{ .split = .{ .a = 0, .b = 0 } });
                        splits[ns] = s;
                        ns += 1;
                        g.prog[s].split.a = g.here();
                    }
                    branch_start[i] = g.here();
                    try g.gen(b);
                    const j = try g.emit(.{ .jmp = 0 });
                    jmps[nj] = j;
                    nj += 1;
                }
                const end = g.here();
                for (jmps[0..nj]) |j| g.prog[j].jmp = end;
                // The split guarding branch k jumps to branch k+1's start.
                for (splits[0..ns], 0..) |s, k| {
                    g.prog[s].split.b = branch_start[k + 1];
                }
            },
            .star => |child| {
                // s: split(s+1, end); child; jmp s; end:
                const s = try g.emit(.{ .split = .{ .a = 0, .b = 0 } });
                g.prog[s].split.a = g.here();
                try g.gen(child);
                _ = try g.emit(.{ .jmp = s });
                g.prog[s].split.b = g.here();
            },
            .plus => |child| {
                const body = g.here();
                try g.gen(child);
                const s = try g.emit(.{ .split = .{ .a = body, .b = 0 } });
                g.prog[s].split.b = g.here();
            },
            .opt => |child| {
                const s = try g.emit(.{ .split = .{ .a = 0, .b = 0 } });
                g.prog[s].split.a = g.here();
                try g.gen(child);
                g.prog[s].split.b = g.here();
            },
            .repeat => |rep| {
                var i: u32 = 0;
                while (i < rep.min) : (i += 1) try g.gen(rep.child);
                if (rep.max) |mx| {
                    var opt = mx - rep.min;
                    while (opt > 0) : (opt -= 1) {
                        const s = try g.emit(.{ .split = .{ .a = 0, .b = 0 } });
                        g.prog[s].split.a = g.here();
                        try g.gen(rep.child);
                        g.prog[s].split.b = g.here();
                    }
                } else {
                    const star_node = Node{ .star = rep.child };
                    try g.gen(&star_node);
                }
            },
        }
    }
};

// ---- compiled regex + Pike VM ----

pub const Regex = struct {
    pattern: []const u8 = "",
    prog: []Inst,
    /// Simulation entry point: 0 for the search prefix loop, or past it
    /// when the pattern is anchored (`^...`), skipping useless thread
    /// spawns at every input position.
    entry: u32 = 0,
    /// Allocator that owns this regex (for the lazy Pike-VM scratch).
    owner: ?std.mem.Allocator = null,
    /// Lazily allocated simulation scratch, reused across matches. Makes
    /// `matches` non-thread-safe per Regex (compile one per thread).
    scratch: ?*Scratch = null,

    /// Compile a pattern. The program is allocated in `alloc`.
    pub fn compile(alloc: std.mem.Allocator, pattern: []const u8) Error!Regex {
        // Parse into a scratch arena: the AST is built with interior
        // pointers, which an arena keeps stable (its ArrayList growth never
        // moves prior allocations).
        var arena = std.heap.ArenaAllocator.init(alloc);
        defer arena.deinit();
        var p = PatternParser{ .pat = pattern, .alloc = arena.allocator() };
        const root = try p.parseAlternation();
        if (p.pos != pattern.len) return error.InvalidPattern;

        var g = CodeGen{ .prog = try alloc.alloc(Inst, max_program) };
        errdefer alloc.free(g.prog);
        // Reserve the search-prefix slots (split / any-byte / jmp); they
        // are filled below unless the pattern is anchored.
        const s = try g.emit(.{ .split = .{ .a = 0, .b = 0 } });
        _ = try g.emit(.{ .char = .{ .set = .{ ~@as(u64, 0), ~@as(u64, 0) }, .any_high = true } });
        _ = try g.emit(.{ .jmp = s });
        g.prog[s].split.b = g.here();
        const pat_start = g.here();
        try g.gen(root);
        _ = try g.emit(.{ .match = {} });
        var entry: u32 = 0;
        if (pat_start < g.len and g.prog[pat_start] == .bol) {
            // Anchored pattern: start directly at the pattern, skipping the
            // search loop (threads from other positions would die at `^`).
            entry = pat_start;
        } else {
            // Search prefix: loop over any byte so one pass covers all
            // start positions.
            g.prog[s].split.a = s + 1;
        }
        const prog = try alloc.realloc(g.prog, g.len);
        return .{ .pattern = pattern, .prog = prog, .entry = entry, .owner = alloc };
    }

    pub fn deinit(r: *Regex, alloc: std.mem.Allocator) void {
        alloc.free(r.prog);
        if (r.scratch) |s| {
            if (r.owner) |o| {
                o.free(s.dense[0]);
                o.free(s.seen[0]);
                o.free(s.dense[1]);
                o.free(s.seen[1]);
                o.destroy(s);
            }
        }
        r.scratch = null;
    }

    /// Pike-VM scratch: two thread lists with generation-stamped `seen`
    /// arrays (no clearing between steps), swapped by index.
    pub const Scratch = struct {
        dense: [2][]u32,
        seen: [2][]u32,
        gen: [2]u32,
        count: [2]usize,
        cur: usize,
    };

    fn ensureScratch(r: *Regex) Error!void {
        if (r.scratch != null) return;
        const alloc = r.owner orelse return error.OutOfMemory;
        const s = try alloc.create(Scratch);
        errdefer alloc.destroy(s);
        const n = r.prog.len;
        s.* = .{
            .dense = .{ try alloc.alloc(u32, n), try alloc.alloc(u32, n) },
            .seen = .{ try alloc.alloc(u32, n), try alloc.alloc(u32, n) },
            .gen = .{ 1, 1 },
            .count = .{ 0, 0 },
            .cur = 0,
        };
        @memset(s.seen[0], 0);
        @memset(s.seen[1], 0);
        r.scratch = s;
    }

    /// Report whether the regex matches anywhere in `input`.
    pub fn matches(r: *Regex, input: []const u8) bool {
        if (r.prog.len == 0) return false;
        r.ensureScratch() catch return false;
        const s = r.scratch.?;
        var a = s.cur;
        s.gen[a] +%= 1;
        s.count[a] = 0;
        addThread(r, s, a, r.entry, input, 0);
        var sp: usize = 0;
        while (true) {
            const count = s.count[a];
            const dense = s.dense[a];
            for (dense[0..count]) |pc| {
                if (r.prog[pc] == .match) return true;
            }
            if (sp >= input.len or count == 0) return false;
            const c = input[sp];
            const b = 1 - a;
            s.gen[b] +%= 1;
            s.count[b] = 0;
            for (dense[0..count]) |pc| {
                switch (r.prog[pc]) {
                    .char => |cc| {
                        if (bitsetHas(cc.set, c) or (cc.any_high and c >= 0x80)) {
                            addThread(r, s, b, pc + 1, input, sp + 1);
                        }
                    },
                    .range => |br| {
                        if (c >= br.lo and c <= br.hi) {
                            addThread(r, s, b, pc + 1, input, sp + 1);
                        }
                    },
                    else => {},
                }
            }
            a = b;
            sp += 1;
        }
    }

    fn isWordChar(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
    }

    /// Epsilon closure with an explicit stack (bounded by program size).
    fn addThread(r: *Regex, s: *Scratch, list: usize, pc_in: u32, input: []const u8, pos: usize) void {
        var stack: [512]u32 = undefined;
        var depth: usize = 0;
        stack[depth] = pc_in;
        depth += 1;
        while (depth > 0) {
            depth -= 1;
            const pc = stack[depth];
            if (s.seen[list][pc] == s.gen[list]) continue;
            s.seen[list][pc] = s.gen[list];
            s.dense[list][s.count[list]] = pc;
            s.count[list] += 1;
            switch (r.prog[pc]) {
                .jmp => |t| {
                    if (depth < stack.len) {
                        stack[depth] = t;
                        depth += 1;
                    }
                },
                .split => |sp2| {
                    if (depth < stack.len) {
                        stack[depth] = sp2.b;
                        depth += 1;
                    }
                    if (depth < stack.len) {
                        stack[depth] = sp2.a;
                        depth += 1;
                    }
                },
                .bol => {
                    if (pos == 0 and depth < stack.len) {
                        stack[depth] = pc + 1;
                        depth += 1;
                    }
                },
                .eol => {
                    if (pos == input.len and depth < stack.len) {
                        stack[depth] = pc + 1;
                        depth += 1;
                    }
                },
                .word_boundary => |want| {
                    const before = pos > 0 and isWordChar(input[pos - 1]);
                    const after = pos < input.len and isWordChar(input[pos]);
                    const boundary = before != after;
                    if (boundary == want and depth < stack.len) {
                        stack[depth] = pc + 1;
                        depth += 1;
                    }
                },
                else => {},
            }
        }
    }
};

test {
    _ = tests;
}

const tests = struct {
    const testing = std.testing;

    fn m(pattern: []const u8, input: []const u8) !bool {
        var re = try Regex.compile(testing.allocator, pattern);
        defer re.deinit(testing.allocator);
        return re.matches(input);
    }

    test "literals and dot" {
        try testing.expect(try m("ubb", "hubba"));
        try testing.expect(!try m("ubb", "humba"));
        try testing.expect(try m("", "cows"));
        try testing.expect(try m("bc", "abcd"));
        try testing.expect(try m("a.c", "abc"));
        try testing.expect(!try m("a.c", "ac"));
        try testing.expect(!try m("a.c", "a\nc"));
    }

    test "alternation and groups" {
        try testing.expect(try m("foo|bar", "xfoox"));
        try testing.expect(try m("gr(a|e)y", "grey"));
        try testing.expect(try m("gr(a|e)y", "gray"));
        try testing.expect(!try m("gr(a|e)y", "groy"));
        try testing.expect(try m("(a|b)+", "cbab"));
        try testing.expect(try m("ba(na)*", "banana"));
        try testing.expect(try m("ba(na)*", "ba"));
        try testing.expect(!try m("ba(na)+", "ba"));
        try testing.expect(try m("^(a|b)*$", "abab"));
        try testing.expect(!try m("^(a|b)*$", "aba c"));
    }

    test "classes" {
        try testing.expect(try m("[xyz]+", "zzyyx"));
        try testing.expect(!try m("[abc]+$", "zzyyx"));
        try testing.expect(try m("[abc]+", "xaybc"));
        try testing.expect(!try m("[abc]+", "zzyyx"));
        try testing.expect(try m("[a-z]+", "123abc"));
        try testing.expect(try m("[^0-9]+", "abc"));
        try testing.expect(!try m("^[^0-9]+$", "ab1c"));
        try testing.expect(try m("\\d+", "abc123"));
        try testing.expect(try m("\\w+", "  hi_ "));
        try testing.expect(try m("\\s", "a b"));
        try testing.expect(try m("[]a]", "]"));
    }

    test "anchors and boundaries" {
        try testing.expect(try m("^abc", "abcdef"));
        try testing.expect(!try m("^abc", "xabc"));
        try testing.expect(try m("def$", "abcdef"));
        try testing.expect(!try m("def$", "defx"));
        try testing.expect(try m("^$", ""));
        try testing.expect(try m("\\bfoo\\b", "a foo b"));
        try testing.expect(!try m("\\bfoo\\b", "afoob"));
        try testing.expect(!try m("^abc$", "abcd"));
    }

    test "quantifiers" {
        try testing.expect(try m("a{2,3}", "caaat"));
        try testing.expect(!try m("^a{2,3}$", "aaaa"));
        try testing.expect(try m("^a{2,}$", "aaaa"));
        try testing.expect(!try m("^a{2,}$", "a"));
        try testing.expect(try m("^a?$", ""));
        try testing.expect(try m("^a?$", "a"));
        try testing.expect(!try m("^a?$", "aa"));
        try testing.expect(try m("colou?r", "color"));
        try testing.expect(try m("colou?r", "colour"));
        try testing.expect(!try m("^a{0}$", "a"));
    }

    test "unicode" {
        try testing.expect(try m("a+", "mañana"));
        try testing.expect(try m("ñ+", "mañana"));
        try testing.expect(try m("(a|😀){2}", "🐱😀😀"));
        try testing.expect(!try m("(a|😀){2}", "🐱😀"));
        try testing.expect(try m("^.$", "😀"));
        try testing.expect(!try m("^..$", "😀"));
        try testing.expect(try m("^.$", "é"));
    }

    test "invalid patterns" {
        try testing.expectError(error.InvalidPattern, Regex.compile(testing.allocator, "("));
        try testing.expectError(error.InvalidPattern, Regex.compile(testing.allocator, "a)"));
        try testing.expectError(error.InvalidPattern, Regex.compile(testing.allocator, "[a"));
        try testing.expectError(error.InvalidPattern, Regex.compile(testing.allocator, "*a"));
    }

    test "linear time on adversarial input" {
        // Classic catastrophic-backtracking pattern for backtracking engines.
        var re = try Regex.compile(testing.allocator, "(a+)+b");
        defer re.deinit(testing.allocator);
        const big = try testing.allocator.alloc(u8, 500);
        defer testing.allocator.free(big);
        @memset(big, 'a');
        try testing.expect(!re.matches(big));
    }
};
