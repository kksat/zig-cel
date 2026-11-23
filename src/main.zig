//! CEL command-line tool.
//!
//! Usage:
//!   cel eval [--bind name=value]... 'expression'
//!   cel run   [--bind name=value]... 'expression'   (alias for eval)
//!
//! Values in --bind are parsed as CEL literals, e.g. --bind x=42,
//! --bind name='"hello"', --bind items='[1, 2, 3]'.

const std = @import("std");
const cel = @import("cel");
const eval = cel.eval;
const parser = cel.parser;
const value_mod = cel.value;

const Value = value_mod.Value;

fn parseBindingValue(alloc: std.mem.Allocator, text: []const u8) !Value {
    // Parse the value text as a CEL expression and evaluate it with no
    // bindings; the result must be a value literal.
    const r = try parser.parseSrc(alloc, text, .{});
    const tree = switch (r) {
        .ok => |t| t,
        .err => return error.InvalidBinding,
    };
    var ev = eval.Evaluator.init(alloc, tree);
    return ev.run(&.{}) catch return error.InvalidBinding;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();
    _ = args.next(); // program name

    var expr: ?[]const u8 = null;
    var bindings = std.ArrayList(eval.Binding){};
    defer bindings.deinit(alloc);
    // Arena that keeps binding values alive for the whole run.
    var bind_arena = std.heap.ArenaAllocator.init(alloc);
    defer bind_arena.deinit();

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "eval") or std.mem.eql(u8, arg, "run")) {
            continue; // subcommand
        }
        if (std.mem.eql(u8, arg, "--bind")) {
            const kv = args.next() orelse {
                std.debug.print("--bind requires name=value\n", .{});
                std.process.exit(2);
            };
            const eq = std.mem.indexOfScalar(u8, kv, '=') orelse {
                std.debug.print("--bind requires name=value\n", .{});
                std.process.exit(2);
            };
            var arena = std.heap.ArenaAllocator.init(alloc);
            defer arena.deinit();
            const v = parseBindingValue(arena.allocator(), kv[eq + 1 ..]) catch {
                std.debug.print("invalid binding value: {s}\n", .{kv[eq + 1 ..]});
                std.process.exit(2);
            };
            // Deep-copy into the persistent binding arena so list/map
            // payloads survive.
            const persistent = try deepCopyValue(bind_arena.allocator(), v);
            try bindings.append(alloc, .{ .name = kv[0..eq], .value = persistent });
        } else if (expr == null) {
            expr = arg;
        } else {
            std.debug.print("unexpected argument: {s}\n", .{arg});
            std.process.exit(2);
        }
    }

    const src = expr orelse {
        std.debug.print("usage: cel eval [--bind name=value]... 'expression'\n", .{});
        std.process.exit(2);
    };

    var arena = std.heap.ArenaAllocator.init(alloc);
    const a = arena.allocator();

    const r = try parser.parseSrc(a, src, .{});
    const tree = switch (r) {
        .ok => |t| t,
        .err => |e| {
            std.debug.print("parse error: {s} (at offset {d})\n", .{ e.msg, e.pos });
            std.process.exit(1);
        },
    };
    const prog = cel.vm.Compiler.compile(a, tree) catch {
        std.debug.print("compile error\n", .{});
        std.process.exit(1);
    };
    const v = switch (try prog.run(a, bindings.items, "")) {
        .value => |v| v,
        .err => |e| {
            std.debug.print("error: {s}\n", .{e});
            std.process.exit(1);
        },
    };

    var aw = std.Io.Writer.Allocating.init(a);
    try v.format(&aw.writer);
    var stdout = std.fs.File.stdout().writer(&.{});
    try stdout.interface.writeAll(aw.written());
    try stdout.interface.writeAll("\n");
}

/// Deep-copy a value (strings, bytes, lists, maps) into another allocator.
fn deepCopyValue(alloc: std.mem.Allocator, v: Value) !Value {
    switch (v) {
        .string_t => |s| return .{ .string_t = try alloc.dupe(u8, s) },
        .bytes_t => |s| return .{ .bytes_t = try alloc.dupe(u8, s) },
        .list_t => |l| {
            const out = try alloc.alloc(Value, l.len);
            for (l, 0..) |item, i| out[i] = try deepCopyValue(alloc, item);
            return .{ .list_t = out };
        },
        .map_t => |m| {
            const entries = try alloc.alloc(value_mod.Map.Entry, m.entries.len);
            for (m.entries, 0..) |e, i| {
                entries[i] = .{
                    .key = try deepCopyValue(alloc, e.key),
                    .value = try deepCopyValue(alloc, e.value),
                };
            }
            const out = try value_mod.Map.init(alloc, entries);
            return .{ .map_t = out };
        },
        else => return v,
    }
}
