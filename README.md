# zig-cel

A [CEL (Common Expression Language)](https://github.com/cel-expr/cel-spec)
implementation in Zig: lexer, parser, macro expansion, a bytecode compiler,
and a stack-VM evaluator, validated against the official `cel-spec` simple
conformance suite.

CEL is a small, side-effect-free, terminating expression language for
untrusted code. This project implements the language (values, operators,
standard functions, macros) with a focus on **low evaluation latency and low
memory usage**:

- one-time parse + compile to a flat instruction stream; evaluate many
  times;
- compile-time constant folding (literal arithmetic, list/map/string
  construction, regex pooling for constant patterns);
- interned function dispatch with fast paths for `==`, int ordering,
  string functions, `in`, and `size`;
- comprehension variables compiled to lexical slots (no name lookup in
  loops);
- program-owned operand stack / locals / argument buffers, reused across
  runs: steady-state evaluation of expressions that construct no
  lists/maps/strings performs **zero heap allocations**;
- a Pike-VM regex engine (RE2 syntax subset) with linear-time matching and
  anchored-pattern fast paths;
- heterogeneous runtime semantics per the spec (cross-type numeric
  equality/ordering via int→double cast, commutative `&&`/`||` error
  absorption, `?:` short-circuiting, container-based name resolution).

## Performance

Benchmarks (`zig build -Doptimize=ReleaseFast bench`, batched rdtsc
timing, single thread, AMD64) versus
[cel-go](https://github.com/cel-expr/cel-go) v0.33.4 (Go 1.27, typed
environment, `Program.Eval`) on the same corpus and machine:

| case                    | cel-go (min) | zig-cel (min) | speedup | cel-go allocs/op | zig-cel allocs/op |
| ----------------------- | -----------: | ------------: | ------: | ---------------: | ----------------: |
| literal arithmetic      |    154 ns    |     20 ns     |  7.7x   |       0          |        0          |
| variable comparison     |    129 ns    |     74 ns     |  1.7x   |       0          |        0          |
| string ops (3 calls)    |    223 ns    |    118 ns     |  1.9x   |       3          |        0          |
| anchored regex match    |  3,879 ns    |    361 ns     | 10.7x   |      64          |        0          |
| map access              |    208 ns    |    133 ns     |  1.6x   |       1          |        0          |
| ternary chain           |    199 ns    |     91 ns     |  2.2x   |       0          |        0          |
| `all` over 10 elements  |  1,258 ns    |    256 ns     |  4.9x   |       1          |        0          |
| `exists` over 10        |    912 ns    |    273 ns     |  3.3x   |       1          |        0          |
| `filter` + `map`        |  3,444 ns    |    754 ns     |  4.6x   |      37          |        0*         |
| nested macros (10x10)   |  9,487 ns    |  2,300 ns     |  4.1x   |      11          |        0          |
| conversions             |    287 ns    |     44 ns     |  6.5x   |       2          |        0          |
| size checks             |    210 ns    |    130 ns     |  1.6x   |       2          |        0          |
| math.greatest (list)    |    461 ns    |    232 ns     |  2.0x   |       2          |        0          |
| math.greatest (4 args)  |    360 ns    |    178 ns     |  2.0x   |       3          |        0          |
| math bitwise ops        |    203 ns    |    123 ns     |  1.7x   |       0          |        0          |
| charAt + indexOf        |    370 ns    |    159 ns     |  2.3x   |       4          |        0          |
| string.format           |  1,225 ns    |    431 ns     |  2.8x   |      21          |        0*         |
| split + join            |    585 ns    |    231 ns     |  2.5x   |       9          |        0*         |

\* aggregate outputs are allocated in the caller's per-run arena (bump
allocation); no allocator round-trips in steady state.

Parse: ~3 us/expression; compile: ~3 us/expression (one-time costs).

Notes on methodology and remaining headroom are in `bench/bench.zig` and
the commit history. The evaluator is at a local optimum for its
architecture (per-instruction cost ~6 ns; further gains would require
unboxed value representations with ~20-30% upside).

## Conformance

`zig build conformance` runs the vendored cel-spec simple suites:

```
TOTAL: 1574 passed, 0 failed, 327 skipped (74 message mismatches, non-fatal)
```

Skipped tests require unimplemented features: protocol buffer messages
(`proto2`/`proto3`/`enums`/`wrappers`/`dynamic` JSON values), extension
libraries, unknown-set semantics, or check-phase type deduction. Error
message *content* mismatches are reported but non-fatal (the spec's error
strings are runtime-specific; the official runners do not match them
exactly either).

## Usage

Command line:

    zig build
    ./zig-out/bin/cel eval '1 + 2 * 3 == size([1, 2, 3])'
    ./zig-out/bin/cel eval --bind x=42 --bind items='[1, 2, 3]' \
        'items.filter(x, x > 1).map(x, x * x)'

As a library:

```zig
const std = @import("std");
const cel = @import("cel");

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
defer arena.deinit();
const a = arena.allocator();

// Parse + compile once.
const parsed = cel.parser.parseSrc(a, "size(items) > 3 && items[0] > 0", .{});
const prog = try cel.vm.Compiler.compile(a, parsed.ok);

// Evaluate many times (values constructed during the run live in the
// caller-provided arena).
const result = try prog.run(a, &.{
    .{ .name = "items", .value = .{ .list_t = &items } },
}, "");
```

`Program.run` reuses the program's stack and buffers, so a single Program
must not be evaluated concurrently (compile one per thread). Values built
by a run reference the arena passed to it.

## Status

- Lexer / parser: full CEL grammar including raw, triple-quoted, and bytes
  literals, reserved words (allowed as selectors and receiver functions),
  comments, min-int literal folding.
- Macros: `has`, `all`, `exists`, `exists_one`, `filter`, `map` (both
  forms).
- Values: null, bool, int, uint, double, string, bytes, list, map, type,
  duration, timestamp.
- Standard functions: arithmetic, comparisons, equality, `in`, indexing,
  `size`, `contains`/`startsWith`/`endsWith`/`matches`, conversions,
  timestamp/duration accessors with timezones.
- Extensions (opt-in at parse and evaluation time, like cel-go):
  - **math**: `math.greatest`/`math.least` var-arg macros (expanding to
    `math.@min`/`math.@max` with heterogeneous numeric comparison),
    `ceil`, `floor`, `round`, `trunc`, `abs`, `sign`, `isInf`, `isNaN`,
    `isFinite`, `sqrt`, and the bitwise functions (`bitAnd`, `bitOr`,
    `bitXor`, `bitNot`, `bitShiftLeft`, `bitShiftRight`) with cel-go/Go
    shift semantics.
  - **strings**: `charAt`, `indexOf`, `lastIndexOf`, `lowerAscii`,
    `upperAscii`, `replace`, `split`, `substring`, `trim`, `join`,
    `strings.quote`, `reverse`, and `format` (v2 semantics: unquoted
    strings inside lists/maps, maps sorted by formatted key, and
    Go-`strconv`-compatible `%.Nf`/`%.Ne` rounding via exact decimal
    expansion of the double).
- Not implemented: protocol buffer messages (no protobuf dependency),
  protobuf wrappers, `google.protobuf.Any` unwrapping, extension
  libraries (`cel.bind`, optional types, math/string extensions), the
  optional type-checking phase.

## Development

    zig build test        # unit + conformance tests
    zig build conformance # conformance suite only (also installs cel-conformance)
    zig build -Doptimize=ReleaseFast bench  # latency + memory benchmarks

## License

Apache License 2.0. See [LICENSE](LICENSE). Conformance test data is
vendored from [cel-spec](https://github.com/cel-expr/cel-spec) (Apache-2.0).
