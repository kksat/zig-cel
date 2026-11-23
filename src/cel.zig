//! CEL implementation in Zig (root module).

pub const lexer = @import("lexer.zig");
pub const token = @import("token.zig");
pub const parser = @import("parser.zig");
pub const ast = @import("ast.zig");
pub const value = @import("value.zig");
pub const stdlib = @import("stdlib.zig");
pub const vm = @import("vm.zig");
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
}
