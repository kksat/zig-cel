//! CEL implementation in Zig (root module).

const std = @import("std");

pub const lexer = @import("lexer.zig");
pub const token = @import("token.zig");
pub const parser = @import("parser.zig");
pub const ast = @import("ast.zig");
pub const value = @import("value.zig");
pub const stdlib = @import("stdlib.zig");
pub const vm = @import("vm.zig");
pub const ext = @import("ext.zig");
pub const eval = @import("eval.zig");
pub const regex = @import("regex.zig");
pub const time = @import("time.zig");

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
    _ = @import("value.zig");
    _ = @import("eval.zig");
    _ = @import("regex.zig");
    _ = @import("time.zig");
    _ = @import("stdlib.zig");
    _ = @import("vm.zig");
    _ = @import("ext.zig");
}

// Mirror of the README "As a library" example, so the documented snippet
// cannot silently stop compiling.
test "README usage example" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const parsed = try parser.parseSrc(a, "size(items) > 3 && items[0] > 0", .{});
    const tree = switch (parsed) {
        .ok => |t| t,
        .err => return error.TestUnexpectedResult,
    };
    const prog = try vm.Compiler.compile(a, tree, .{});

    const items = [_]vm.Value{ .{ .int_t = 1 }, .{ .int_t = 2 }, .{ .int_t = 3 } };
    const result = try prog.run(a, &.{
        .{ .name = "items", .value = .{ .list_t = &items } },
    }, "", .{});
    switch (result) {
        .value => |v| try std.testing.expect(v == .bool_t and !v.bool_t),
        .err => return error.TestUnexpectedResult,
    }
}
