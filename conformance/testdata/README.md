# Conformance test data

Files in this directory are copied verbatim from the [cel-spec](https://github.com/cel-expr/cel-spec)
repository (`tests/simple/testdata/`), which is licensed under the Apache
License 2.0. See `LICENSE` in the repository root.

Suites requiring protocol buffer message support (`proto2`, `proto3`,
`enums`, `wrappers`), extension libraries (`*_ext`, `macros2`), unknown-set
semantics (`unknowns`), or check-phase type deduction (`type_deduction`)
are not vendored.

`math_ext` runs with the math extension enabled; `string_ext` runs with the
strings extension enabled.
