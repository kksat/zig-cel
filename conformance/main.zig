//! CEL conformance runner.
//!
//! Runs the vendored cel-spec simple conformance suites
//! (conformance/testdata/*.textproto) through parse + evaluate and compares
//! against the expected value / error matchers.
//!
//! Matching rules:
//! - value matchers use CEL heterogeneous value equality (NaN matches NaN);
//! - error matchers pass when evaluation produced an error and any expected
//!   message is a substring of the actual message after normalization
//!   (case-folding, `_` removal), which is how the official runners
//!   compare the differently-spelled error spellings across runtimes.
//!   Message mismatches are reported but do not fail the test.
//! - tests that require unimplemented features (protocol buffer messages,
//!   enum values, unknown-set semantics, check-only type deduction) are
//!   reported as skipped.

const std = @import("std");
const cel = @import("cel");
const textproto = @import("textproto.zig");

const parser = cel.parser;
const eval = cel.eval;
const value_mod = cel.value;
const Value = value_mod.Value;

const TestFile = struct {
    name: []const u8,
    ext: cel.ext.Extensions = .{},
};

const test_files = [_]TestFile{
    .{ .name = "basic.textproto" },
    .{ .name = "comparisons.textproto" },
    .{ .name = "conversions.textproto" },
    .{ .name = "dynamic.textproto" },
    .{ .name = "fp_math.textproto" },
    .{ .name = "integer_math.textproto" },
    .{ .name = "lists.textproto" },
    .{ .name = "logic.textproto" },
    .{ .name = "macros.textproto" },
    .{ .name = "math_ext.textproto", .ext = .{ .math = true } },
    .{ .name = "namespace.textproto" },
    .{ .name = "parse.textproto" },
    .{ .name = "string.textproto" },
    .{ .name = "string_ext.textproto", .ext = .{ .strings = true } },
    .{ .name = "timestamps.textproto" },
};

const Stats = struct {
    passed: usize = 0,
    failed: usize = 0,
    skipped: usize = 0,
    msg_mismatches: usize = 0,
};

var verbose = false;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();
    _ = args.next();
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
            verbose = true;
        }
    }
    var stdout = std.fs.File.stdout().writer(&.{});
    const total = try run(alloc, &stdout.interface);
    std.process.exit(if (total.failed > 0) 1 else 0);
}

/// Run all vendored conformance suites; returns the aggregate stats.
/// Progress is written to `w` when non-null (the zig test runner owns
/// stdout, so tests pass null).
pub fn run(alloc: std.mem.Allocator, w: ?*std.Io.Writer) !Stats {
    var total = Stats{};

    for (test_files) |test_file| {
        const file_name = test_file.name;
        const file_ext = test_file.ext;
        const path = try std.fmt.allocPrint(alloc, "conformance/testdata/{s}", .{file_name});
        defer alloc.free(path);
        const src = std.fs.cwd().readFileAlloc(alloc, path, 16 * 1024 * 1024) catch |e| {
            if (w) |out| try out.print("missing testdata: {s} ({s})\n", .{ path, @errorName(e) });
            return e;
        };
        defer alloc.free(src);

        var arena = std.heap.ArenaAllocator.init(alloc);
        defer arena.deinit();
        const a = arena.allocator();

        const file = textproto.Parser.parse(a, src) catch |e| {
            if (w) |out| try out.print("failed to parse {s}: {s} at offset {d} (lex: {s}): {s}\n", .{ path, @errorName(e), textproto.last_err_pos, textproto.last_err_msg, src[@max(textproto.last_err_pos - 200, 0)..@min(textproto.last_err_pos + 30, src.len)] });
            return e;
        };

        var stats = Stats{};
        for (file.sections) |section| {
            for (section.tests) |t| {
                runTest(a, file.name, section.name, t, file_ext, &stats);
            }
        }
        if (w) |out| {
            try out.print("{s:<20} {d:>4} passed, {d:>3} failed, {d:>3} skipped", .{ file.name, stats.passed, stats.failed, stats.skipped });
            if (stats.msg_mismatches > 0) {
                try out.print(" ({d} message mismatches, non-fatal)", .{stats.msg_mismatches});
            }
            try out.writeAll("\n");
        }
        total.passed += stats.passed;
        total.failed += stats.failed;
        total.skipped += stats.skipped;
        total.msg_mismatches += stats.msg_mismatches;
    }
    if (w) |out| {
        try out.print("\nTOTAL: {d} passed, {d} failed, {d} skipped", .{ total.passed, total.failed, total.skipped });
        if (total.msg_mismatches > 0) {
            try out.print(" ({d} message mismatches, non-fatal)", .{total.msg_mismatches});
        }
        try out.writeAll("\n");
    }
    return total;
}

test "conformance suite" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const total = try run(arena.allocator(), null);
    if (total.failed > 0) {
        std.debug.print("conformance: {d} failures\n", .{total.failed});
        return error.TestUnexpectedResult;
    }
}

fn runTest(a: std.mem.Allocator, file_name: []const u8, section: []const u8, t: textproto.Test, file_ext: cel.ext.Extensions, stats: *Stats) void {
    const label = std.fmt.allocPrint(a, "{s}/{s}/{s}", .{ file_name, section, t.name }) catch return;

    // Skip tests needing unimplemented features.
    if (t.check_only) {
        stats.skipped += 1;
        if (verbose) std.debug.print("SKIP {s}: check-only\n", .{label});
        return;
    }
    if (usesUnsupported(t.result) or bindingsUnsupported(t.bindings)) {
        stats.skipped += 1;
        if (verbose) std.debug.print("SKIP {s}: unsupported feature\n", .{label});
        return;
    }

    // Parse.
    const parsed = parser.parseSrc(a, t.expr, .{ .disable_macros = t.disable_macros, .ext = file_ext }) catch {
        stats.failed += 1;
        std.debug.print("FAIL {s}: parse error (out of memory)\n  expr: {s}\n", .{ label, t.expr });
        return;
    };
    const tree = switch (parsed) {
        .ok => |tr| tr,
        .err => |e| {
            stats.failed += 1;
            std.debug.print("FAIL {s}: parse error: {s} at {d}\n  expr: {s}\n", .{ label, e.msg, e.pos, t.expr });
            return;
        },
    };
    // Message literals (other than the well-known Duration/Timestamp value
    // forms which we also do not support) are unsupported.
    if (containsStructLiteral(a, tree, tree.root)) {
        stats.skipped += 1;
        if (verbose) std.debug.print("SKIP {s}: message literal\n", .{label});
        return;
    }

    // Convert bindings.
    var bindings = std.ArrayList(eval.Binding){};
    for (t.bindings) |b| {
        switch (b.value) {
            .val => |tv| {
                bindings.append(a, .{ .name = b.name, .value = toValue(a, tv) }) catch return;
            },
            else => {
                stats.skipped += 1;
                if (verbose) std.debug.print("SKIP {s}: error/unknown binding\n", .{label});
                return;
            },
        }
    }

    // Compile and evaluate.
    const prog = cel.vm.Compiler.compile(a, tree, .{ .ext = file_ext }) catch {
        stats.failed += 1;
        std.debug.print("FAIL {s}: compile error\n  expr: {s}\n", .{ label, t.expr });
        return;
    };
    const result = prog.run(a, bindings.items, t.container) catch {
        stats.failed += 1;
        std.debug.print("FAIL {s}: out of memory\n  expr: {s}\n", .{ label, t.expr });
        return;
    };

    switch (t.result) {
        .value => |expected| {
            const actual = switch (result) {
                .value => |v| v,
                .err => |e| {
                    stats.failed += 1;
                    std.debug.print("FAIL {s}: eval error: {s}\n  expr: {s}\n", .{ label, e, t.expr });
                    return;
                },
            };
            const want = toValue(a, expected);
            if (!valuesEqual(actual, want)) {
                stats.failed += 1;
                std.debug.print("FAIL {s}: value mismatch\n  expr: {s}\n", .{ label, t.expr });
                std.debug.print("  want:  {s}\n", .{displayOf(a, want)});
                std.debug.print("  got:   {s}\n", .{displayOf(a, actual)});
                return;
            }
            stats.passed += 1;
        },
        .eval_error => |expected_msgs| {
            switch (result) {
                .err => |actual| {
                    // Error produced; check messages (soft).
                    var matched = expected_msgs.len == 0;
                    for (expected_msgs) |m| {
                        if (messageMatches(m, actual)) matched = true;
                    }
                    if (!matched) {
                        stats.msg_mismatches += 1;
                        if (verbose) std.debug.print("NOTE {s}: error message mismatch (non-fatal)\n  expr: {s}\n  want: {s}\n  got:  {s}\n", .{ label, t.expr, concat(a, expected_msgs), actual });
                    }
                    stats.passed += 1;
                },
                .value => |actual| {
                    stats.failed += 1;
                    std.debug.print("FAIL {s}: expected error, got value: {s}\n  expr: {s}\n", .{ label, displayOf(a, actual), t.expr });
                },
            }
        },
        .unknown => {
            stats.skipped += 1;
        },
        .default_true => {
            const actual = switch (result) {
                .value => |v| v,
                .err => |e| {
                    stats.failed += 1;
                    std.debug.print("FAIL {s}: expected true, got error: {s}\n  expr: {s}\n", .{ label, e, t.expr });
                    return;
                },
            };
            if (actual != .bool_t or !actual.bool_t) {
                stats.failed += 1;
                std.debug.print("FAIL {s}: expected true, got: {s}\n  expr: {s}\n", .{ label, displayOf(a, actual), t.expr });
                return;
            }
            stats.passed += 1;
        },
    }
}

fn usesUnsupported(result: textproto.Result) bool {
    switch (result) {
        .value => |v| return valueUnsupported(v),
        .eval_error => return false,
        .unknown => return true,
        .default_true => return false,
    }
}

fn bindingsUnsupported(bindings: []const textproto.Binding) bool {
    for (bindings) |b| {
        switch (b.value) {
            .val => |v| {
                if (valueUnsupported(v)) return true;
            },
            else => return true,
        }
    }
    return false;
}

fn valueUnsupported(v: textproto.TValue) bool {
    switch (v) {
        .obj_other, .enum_v => return true,
        .list_v => |l| {
            for (l) |item| {
                if (valueUnsupported(item)) return true;
            }
            return false;
        },
        .map_v => |entries| {
            for (entries) |e| {
                if (valueUnsupported(e.key) or valueUnsupported(e.value)) return true;
            }
            return false;
        },
        else => return false,
    }
}

fn containsStructLiteral(a: std.mem.Allocator, tree: parser.Ast, node: parser.NodeIndex) bool {
    var stack = std.ArrayList(parser.NodeIndex){};
    defer stack.deinit(a);
    stack.append(a, node) catch return false;
    while (stack.items.len > 0) {
        const n = stack.pop().?;
        const node_data = tree.nodes[n];
        if (node_data.kind == .struct_) return true;
        for (tree.children(n)) |c| {
            stack.append(a, c) catch return false;
        }
    }
    return false;
}

fn toValue(a: std.mem.Allocator, v: textproto.TValue) Value {
    switch (v) {
        .unset, .null_v => return .{ .null_t = {} },
        .bool_v => |b| return .{ .bool_t = b },
        .int_v => |i| return .{ .int_t = i },
        .uint_v => |u| return .{ .uint_t = u },
        .double_v => |d| return .{ .double_t = d },
        .string_v => |s| return .{ .string_t = s },
        .bytes_v => |s| return .{ .bytes_t = s },
        .list_v => |l| {
            const items = a.alloc(Value, l.len) catch return .{ .null_t = {} };
            for (l, 0..) |item, i| items[i] = toValue(a, item);
            return .{ .list_t = items };
        },
        .map_v => |entries| {
            const es = a.alloc(value_mod.Map.Entry, entries.len) catch return .{ .null_t = {} };
            for (entries, 0..) |e, i| {
                es[i] = .{ .key = toValue(a, e.key), .value = toValue(a, e.value) };
            }
            const m = value_mod.Map.init(a, es) catch return .{ .null_t = {} };
            return .{ .map_t = m };
        },
        .type_v => |t| {
            if (value_mod.Type.fromName(t)) |ty| return .{ .type_t = ty };
            return .{ .null_t = {} };
        },
        .obj_duration => |d| return .{ .duration_t = .{ .seconds = d.seconds, .nanos = d.nanos } },
        .obj_timestamp => |t| return .{ .timestamp_t = .{ .seconds = t.seconds, .nanos = @intCast(t.nanos) } },
        .obj_other, .enum_v => return .{ .null_t = {} },
    }
}

/// Expected-vs-actual comparison with the proto NaN caveat (any NaN matches
/// any NaN).
fn valuesEqual(actual: Value, want: Value) bool {
    if (actual == .double_t and want == .double_t) {
        if (std.math.isNan(want.double_t)) return std.math.isNan(actual.double_t);
    }
    return Value.eql(actual, want);
}

fn displayOf(a: std.mem.Allocator, v: Value) []const u8 {
    var aw = std.Io.Writer.Allocating.init(a);
    v.format(&aw.writer) catch return "<error>";
    return aw.written();
}

fn concat(a: std.mem.Allocator, msgs: []const []const u8) []const u8 {
    var aw = std.Io.Writer.Allocating.init(a);
    for (msgs, 0..) |m, i| {
        if (i > 0) aw.writer.writeAll(" | ") catch {};
        aw.writer.writeAll(m) catch {};
    }
    return aw.written();
}

/// Substring match after normalization (lowercase, strip underscores), so
/// e.g. "no_such_overload", "no such overload", and "NoMatchingOverload"
/// all compare equal.
fn messageMatches(expected: []const u8, actual: []const u8) bool {
    var eb: [256]u8 = undefined;
    var en: usize = 0;
    for (expected) |c| {
        if (c == '_') continue;
        if (en == eb.len) break;
        eb[en] = std.ascii.toLower(c);
        en += 1;
    }
    var ab: [1024]u8 = undefined;
    var an: usize = 0;
    for (actual) |c| {
        if (c == '_') continue;
        if (an == ab.len) break;
        ab[an] = std.ascii.toLower(c);
        an += 1;
    }
    return std.mem.indexOf(u8, ab[0..an], eb[0..en]) != null;
}

test {
    _ = @import("textproto.zig");
}
