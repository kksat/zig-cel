const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Library: CEL compiler + evaluator. The module is registered under the
    // name "cel" so dependents can use `dep.module("cel")` and import it
    // as `@import("cel")`; the static library artifact is for C-style
    // consumers and also installs the module's object code.
    const cel_mod = b.addModule("cel", .{
        .root_source_file = b.path("src/cel.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .name = "cel",
        .root_module = cel_mod,
        .linkage = .static,
    });
    b.installArtifact(lib);

    // CLI tool: `cel eval 'expr'` / `cel repl`.
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.addImport("cel", cel_mod);
    const exe = b.addExecutable(.{ .name = "cel", .root_module = exe_mod });
    b.installArtifact(exe);

    // Unit tests.
    const lib_tests = b.addTest(.{ .root_module = cel_mod });
    const run_lib_tests = b.addRunArtifact(lib_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_tests.step);

    // Conformance suite (runs against vendored cel-spec testdata). The test
    // target executes the full suite; the executable can be run directly
    // (e.g. with -v for per-test skip/failure notes).
    const conf_mod = b.createModule(.{
        .root_source_file = b.path("conformance/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    conf_mod.addImport("cel", cel_mod);
    const conf_tests = b.addTest(.{ .root_module = conf_mod });
    const run_conf = b.addRunArtifact(conf_tests);
    test_step.dependOn(&run_conf.step);
    const conf_exe = b.addExecutable(.{ .name = "cel-conformance", .root_module = conf_mod });
    const install_conf = b.addInstallArtifact(conf_exe, .{});
    const conf_step = b.step("conformance", "Run CEL conformance suite");
    conf_step.dependOn(&install_conf.step);
    conf_step.dependOn(&run_conf.step);

    // Benchmarks (latency + memory).
    const bench_mod = b.createModule(.{
        .root_source_file = b.path("bench/bench.zig"),
        .target = target,
        .optimize = optimize,
    });
    bench_mod.addImport("cel", cel_mod);
    const bench_exe = b.addExecutable(.{ .name = "cel-bench", .root_module = bench_mod });
    const bench_cmd = b.addRunArtifact(bench_exe);
    const bench_step = b.step("bench", "Run CEL benchmarks");
    bench_step.dependOn(&bench_cmd.step);
}
