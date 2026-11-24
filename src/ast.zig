//! CEL AST.
//!
//! Nodes are stored by index (`NodeIndex`) in a flat arena owned by the
//! parsed program; child links live in a side table (`child`, `nchild` point
//! into a contiguous `[]NodeIndex`). This keeps the tree compact and
//! cache-friendly, and makes it cheap to walk for bytecode compilation.

const std = @import("std");

pub const NodeIndex = u32;
pub const no_node: NodeIndex = std.math.maxInt(NodeIndex);

pub const Kind = enum(u8) {
    // Literals.
    int,
    uint,
    double,
    string,
    bytes,
    bool_,
    null_,
    // Expressions.
    ident,
    select,
    call,
    list,
    map,
    /// Message construction `M{f: v}` (parsed; evaluation reports
    /// unsupported since protobuf messages are not implemented).
    struct_,
    /// Logical NOT (`!`), with commutative error semantics.
    not,
    /// Arithmetic negation (`-`).
    neg,
    /// Logical AND (`&&`).
    and_,
    /// Logical OR (`||`).
    or_,
    /// Conditional (`? :`), lazy.
    ternary,
    /// Comprehension macro (all/exists/exists_one/filter/map).
    macro,
};

pub const MacroKind = enum(u8) {
    all,
    exists,
    exists_one,
    filter,
    map,
    /// Three-argument map macro: range.map(x, filter, transform).
    map_filter,
};

pub const Flags = packed struct(u8) {
    /// select: this is the target of `has()`; test presence instead of
    /// reading the value.
    test_only: bool = false,
    /// call: receiver-style invocation (`e.f(args...)`).
    receiver: bool = false,
    /// ident: name was written with a leading dot (`.a`), bypassing
    /// comprehension scopes.
    abs: bool = false,
    _pad: u5 = 0,
};

pub const Payload = union(enum) {
    none: void,
    int: i64,
    uint: u64,
    double: f64,
    str: []const u8,
    macro: MacroKind,
};

pub const Node = struct {
    kind: Kind,
    flags: Flags = .{},
    /// Byte offset of the first token of this expression in the source.
    start: u32 = 0,
    /// Byte offset one past the last token of this expression in the
    /// source (i.e. the span is [start, end); parenthesized expressions
    /// include their parentheses).
    end: u32 = 0,
    /// Index of the first child in the program's child table.
    child: u32 = 0,
    /// Number of children.
    nchild: u32 = 0,
    /// Literal value / identifier / function / field / macro-iteration
    /// variable name.
    payload: Payload = .{ .none = {} },

    pub fn children(n: Node, table: []const NodeIndex) []const NodeIndex {
        return table[n.child..][0..n.nchild];
    }
};

/// A parsed program: the node arena, the child-link table, and the root.
pub const Ast = struct {
    nodes: []const Node,
    child_table: []const NodeIndex,
    root: NodeIndex,

    pub fn node(ast: Ast, i: NodeIndex) Node {
        return ast.nodes[i];
    }

    pub fn children(ast: Ast, i: NodeIndex) []const NodeIndex {
        return ast.nodes[i].children(ast.child_table);
    }
};
