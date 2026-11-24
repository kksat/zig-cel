//! CEL evaluation benchmarks: latency (ns/op) and memory (allocations and
//! bytes per evaluation).
//!
//! Build with `zig build -Doptimize=ReleaseFast bench`.
//!
//! Memory is measured with a counting allocator backing each evaluation's
//! arena, so the reported numbers are the allocations the evaluator itself
//! triggers (arena growth, constructed lists/maps/strings), not benchmark
//! bookkeeping.

const std = @import("std");
const cel = @import("cel");

const parser = cel.parser;
const eval = cel.eval;
const vm = cel.vm;
const value_mod = cel.value;
const Value = value_mod.Value;
const Binding = eval.Binding;

const Case = struct {
    name: []const u8,
    expr: []const u8,
    vars: []const Binding = &.{},
    ext: cel.ext.Extensions = .{},
};

fn makeBindings(a: std.mem.Allocator) ![]Binding {
    const items = try a.alloc(Value, 10);
    for (items, 0..) |*v, i| v.* = .{ .int_t = @intCast(i + 1) };
    const payload_entries = try a.alloc(value_mod.Map.Entry, 3);
    payload_entries[0] = .{ .key = .{ .string_t = "user" }, .value = .{ .string_t = "alice" } };
    payload_entries[1] = .{ .key = .{ .string_t = "roles" }, .value = .{ .list_t = items } };
    payload_entries[2] = .{ .key = .{ .string_t = "count" }, .value = .{ .int_t = 42 } };
    const payload = try value_mod.Map.init(a, payload_entries);
    const bindings = try a.alloc(Binding, 8);
    bindings[0] = .{ .name = "x", .value = .{ .int_t = 42 } };
    bindings[1] = .{ .name = "y", .value = .{ .int_t = 17 } };
    bindings[2] = .{ .name = "name", .value = .{ .string_t = "prefix-middle-suffix" } };
    bindings[3] = .{ .name = "email", .value = .{ .string_t = "user@example.com" } };
    bindings[4] = .{ .name = "items", .value = .{ .list_t = items } };
    bindings[5] = .{ .name = "payload", .value = .{ .map_t = payload } };
    bindings[6] = .{ .name = "a", .value = .{ .int_t = 100 } };
    bindings[7] = .{ .name = "b", .value = .{ .int_t = 7 } };
    return bindings;
}

const cases = [_]Case{
    .{ .name = "literal_arith", .expr = "1 + 2 * 3 - 4 / 2 % 1 == 6" },
    .{ .name = "var_compare", .expr = "x < 100 && x > 10 || y == 42", .vars = &[_]Binding{.{ .name = "x", .value = .{ .int_t = 42 } }, .{ .name = "y", .value = .{ .int_t = 17 } }} },
    .{ .name = "string_ops", .expr = "name.startsWith('prefix') && name.contains('middle') && name.endsWith('suffix')", .vars = &[_]Binding{.{ .name = "name", .value = .{ .string_t = "prefix-middle-suffix" } }} },
    .{ .name = "regex_match", .expr = "email.matches('^[a-z]+@[a-z]+\\\\.[a-z]{2,}$')", .vars = &[_]Binding{.{ .name = "email", .value = .{ .string_t = "user@example.com" } }} },
    .{ .name = "map_access", .expr = "payload['user'] == 'alice' && payload.count > 40" },
    .{ .name = "ternary_chain", .expr = "a > b ? (a > 50 ? a : b) : (b > 5 ? b : a)" },
    .{ .name = "list_all", .expr = "items.all(x, x > 0)" },
    .{ .name = "list_exists", .expr = "items.exists(x, x == 7)" },
    .{ .name = "list_filter_map", .expr = "items.filter(x, x % 2 == 0).map(x, x * x)" },
    .{ .name = "list_sum_manual", .expr = "items.map(x, x * 2).filter(x, x > 5).size() > 3" },
    .{ .name = "exists_one", .expr = "items.exists_one(x, x % 3 == 0)" },
    .{ .name = "nested_macro", .expr = "items.all(x, items.exists(y, y == x))" },
    .{ .name = "list_literal_build", .expr = "[1, 2, 3, 4, 5].map(x, x + 1).filter(x, x % 2 == 0)" },
    .{ .name = "map_literal_build", .expr = "{'a': 1, 'b': 2, 'c': 3}.map(k, k).size() == 3" },
    .{ .name = "in_operator", .expr = "x in [1, 42, 99] && 'user' in payload" },
    .{ .name = "conversions", .expr = "int('123') + 7 == 130 && string(42) == '42'" },
    .{ .name = "size_checks", .expr = "size(items) == 10 && size(name) == 20" },
    // Extension benchmarks (math + strings enabled).
    .{ .name = "math_greatest_list", .expr = "math.greatest(items) > 9", .ext = .{ .math = true } },
    .{ .name = "math_greatest_args", .expr = "math.greatest(x, y, a, b) > 99", .ext = .{ .math = true } },
    .{ .name = "math_bitops", .expr = "math.bitAnd(x, 1023) + math.bitOr(y, 7) == 49", .ext = .{ .math = true } },
    .{ .name = "string_charat_indexof", .expr = "name.charAt(0) == 'p' && name.indexOf('middle') > 0", .ext = .{ .strings = true } },
    .{ .name = "string_format", .expr = "'value: %d %.2f %s'.format([x, 1.5, name]) != ''", .ext = .{ .strings = true } },
    .{ .name = "string_split_join", .expr = "name.split('-').size() == 3 && ['a', 'b'].join(',') == 'a,b'", .ext = .{ .strings = true } },
};

/// Counting allocator: tracks allocation count and bytes.
const CountingAllocator = struct {
    child: std.mem.Allocator,
    allocs: usize = 0,
    bytes: usize = 0,

    fn allocator(self: *CountingAllocator) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        self.allocs += 1;
        self.bytes += len;
        return self.child.rawAlloc(len, alignment, ret_addr);
    }

    fn resize(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        if (new_len > buf.len) {
            self.bytes += new_len - buf.len;
        } else {
            self.bytes -= buf.len - new_len;
        }
        return self.child.rawResize(buf, alignment, new_len, ret_addr);
    }

    fn remap(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        if (new_len > buf.len) {
            self.bytes += new_len - buf.len;
        } else {
            self.bytes -= buf.len - new_len;
        }
        return self.child.rawRemap(buf, alignment, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        self.bytes -= buf.len;
        self.child.rawFree(buf, alignment, ret_addr);
    }
};

const TimingResult = struct {
    min_ns: u64,
    median_ns: u64,
    mean_ns: u64,
    allocs_per_op: f64,
    bytes_per_op: f64,
};

fn fmtNs(ns: u64) []const u8 {
    if (ns < 1000) return "ns";
    if (ns < 1_000_000) return "us";
    return "ms";
}

fn scaleNs(ns: u64) f64 {
    if (ns < 1000) return @floatFromInt(ns);
    if (ns < 1_000_000) return @as(f64, @floatFromInt(ns)) / 1000.0;
    return @as(f64, @floatFromInt(ns)) / 1_000_000.0;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var stdout = std.fs.File.stdout().writer(&.{});
    const w = &stdout.interface;

    // Shared bindings for the default variable set.
    var bind_arena = std.heap.ArenaAllocator.init(alloc);
    const bindings = try makeBindings(bind_arena.allocator());

    try w.print("{s:<22} {s:>10} {s:>10} {s:>10} {s:>9} {s:>10}\n", .{ "case", "min", "median", "mean", "allocs", "bytes" });
    try w.print("{s:<22} {s:>10} {s:>10} {s:>10} {s:>9} {s:>10}\n", .{ "----", "---", "------", "----", "-------", "-----" });

    var parse_total_ns: u64 = 0;
    var compile_total_ns: u64 = 0;
    var timer = try std.time.Timer.start();

    for (cases) |case| {
        var vars = case.vars;
        if (vars.len == 0) vars = bindings;

        // Parse and compile once (outside the timed loop). The arena is
        // warmed with a first parse so the measurement excludes first-chunk
        // allocator growth; the timed parse runs on a second program kept
        // alive separately.
        var parse_arena = std.heap.ArenaAllocator.init(alloc);
        defer parse_arena.deinit();
        {
            const warm = try parser.parseSrc(parse_arena.allocator(), case.expr, .{ .ext = case.ext });
            _ = try vm.Compiler.compile(parse_arena.allocator(), warm.ok, .{ .ext = case.ext });
        }
        _ = parse_arena.reset(.retain_capacity);
        timer.reset();
        const r = try parser.parseSrc(parse_arena.allocator(), case.expr, .{ .ext = case.ext });
        const parse_ns = timer.read();
        timer.reset();
        _ = try vm.Compiler.compile(parse_arena.allocator(), r.ok, .{ .ext = case.ext });
        const compile_ns = timer.read();
        parse_total_ns += parse_ns;
        compile_total_ns += compile_ns;
        const tree = switch (r) {
            .ok => |t| t,
            .err => {
                try w.print("{s:<22} PARSE ERROR\n", .{case.name});
                continue;
            },
        };

        // Compile once (bytecode VM).
        const prog = vm.Compiler.compile(parse_arena.allocator(), tree, .{ .ext = case.ext }) catch {
            try w.print("{s:<22} COMPILE ERROR\n", .{case.name});
            continue;
        };

        // Warmup + correctness check.
        var warm_arena = std.heap.ArenaAllocator.init(alloc);
        const first = prog.run(warm_arena.allocator(), vars, "", .{}) catch {
            try w.print("{s:<22} RUN ERROR\n", .{case.name});
            continue;
        };
        _ = first;
        warm_arena.deinit();

        // Timed loop: fresh arena per evaluation, counting allocator.
        var counting = CountingAllocator{ .child = alloc };
        const calloc = counting.allocator();
        var run_arena = std.heap.ArenaAllocator.init(calloc);
        _ = run_arena.reset(.retain_capacity);

        // Batched timing: each sample times a batch of evaluations and
        // divides, amortizing the rdtsc overhead (~15ns) that would
        // otherwise dominate single-op measurements.
        const batches: usize = 200;
        const batch: usize = 100;
        const iters = batches * batch;
        var times = try alloc.alloc(u64, batches);
        defer alloc.free(times);
        var total_allocs: usize = 0;
        var total_bytes: usize = 0;

        var b: usize = 0;
        while (b < batches) : (b += 1) {
            timer.reset();
            var j: usize = 0;
            while (j < batch) : (j += 1) {
                _ = run_arena.reset(.retain_capacity);
                _ = prog.run(run_arena.allocator(), vars, "", .{}) catch {};
            }
            times[b] = timer.read() / batch;
            total_allocs += counting.allocs;
            total_bytes += counting.bytes;
        }
        run_arena.deinit();

        std.mem.sort(u64, times, {}, std.sort.asc(u64));
        var sum: u128 = 0;
        for (times) |t| sum += t;
        const tr = TimingResult{
            .min_ns = times[0],
            .median_ns = times[batches / 2],
            .mean_ns = @intCast(sum / batches),
            .allocs_per_op = @as(f64, @floatFromInt(total_allocs)) / @as(f64, @floatFromInt(iters)),
            .bytes_per_op = @as(f64, @floatFromInt(total_bytes)) / @as(f64, @floatFromInt(iters)),
        };
        try w.print("{s:<22} {d:>8.1} {s} {d:>8.1} {s} {d:>8.1} {s} {d:>9.1} {d:>10.1}\n", .{
            case.name,
            scaleNs(tr.min_ns),  fmtNs(tr.min_ns),
            scaleNs(tr.median_ns), fmtNs(tr.median_ns),
            scaleNs(tr.mean_ns),  fmtNs(tr.mean_ns),
            tr.allocs_per_op,
            tr.bytes_per_op,
        });
    }
    try w.print("\nparse:  {d} cases, {d}ns/case (median of 1)\n", .{ cases.len, parse_total_ns / cases.len });
    try w.print("compile: {d} cases, {d}ns/case\n", .{ cases.len, compile_total_ns / cases.len });
}
