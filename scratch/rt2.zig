const std = @import("std");
const regex = @import("regex.zig");
pub fn main() !void {
    var re = try regex.Regex.compile(std.heap.page_allocator, "^(a|b)*$");
    std.debug.print("compiled {} insts\n", .{re.prog.len});
    const r = re.matches("abab");
    std.debug.print("abab => {}\n", .{r});
}
