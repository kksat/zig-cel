# zig-cel

A [CEL (Common Expression Language)](https://github.com/cel-expr/cel-spec)
implementation in Zig: lexer, parser, macro expansion, and a bytecode-compiled
evaluator, with a conformance runner against the official `cel-spec` simple
test suite.

CEL is a small, side-effect-free, terminating expression language for
untrusted code. This project implements the language (values, operators,
standard functions, macros) with a focus on **low evaluation latency and low
memory usage**:

- arena-allocated AST and bytecode: parse/compile once, evaluate many times
  with zero per-evaluation heap traffic for typical expressions;
- zero-copy string/bytes literals (slices reference source text when no
  escape processing is needed);
- compile-time constant folding;
- a flat stack VM with a contiguous instruction stream (no interpreter
  recursion);
- heterogeneous runtime semantics per the spec (cross-type numeric
  equality/ordering, commutative `&&`/`||` error absorption, `?:`
  short-circuiting).

## Status

- Lexer / parser: full CEL grammar including raw, triple-quoted, and bytes
  literals, reserved words, comments.
- Macros: `has`, `all`, `exists`, `exists_one`, `filter`, `map` (both forms).
- Values: null, bool, int, uint, double, string, bytes, list, map, type,
  duration, timestamp.
- Standard functions: arithmetic, comparisons, equality, `in`, `size`,
  `contains`/`startsWith`/`endsWith`/`matches`, conversions
  (`int`, `uint`, `double`, `string`, `bool`, `bytes`, `type`, `duration`,
  `timestamp`), `dyn`.
- Not implemented: protocol buffer messages (no protobuf dependency),
  protobuf wrappers, `google.protobuf.Any` unwrapping, extension libraries
  (`cel.bind`, optional types, math/string extensions).

## Usage

Command line:

    zig build
    ./zig-out/bin/cel eval '1 + 2 * 3 == size([1, 2, 3])'
    ./zig-out/bin/cel eval --bind x=3 'x * x'

As a library:

```zig
const std = @import("std");
const cel = @import("cel");

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
defer arena.deinit();

const env = cel.Env.init(arena.allocator());
const prog = try cel.compile(env, "size(items) > 3 && items[0].startsWith('a')");
var out: cel.Value = undefined;
const result = prog.run(env, .{ .vars = &.{
    .{ .name = "items", .value = cel.Value.ofList(...) },
} }, &out);
```

## Development

    zig build test        # unit + conformance tests
    zig build conformance # conformance suite only
    zig build bench       # latency + memory benchmarks

## License

Apache License 2.0. See [LICENSE](LICENSE).
