const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "ts-parser-zig",
        .root_source_file = .{ .path = b.pathFromRoot("src/root.zig") },
        .target = target,
        .optimize = optimize,
    });

    const lib_wasm = b.addSharedLibrary(.{
        .name = "ts-parser-zig",
        .root_source_file = .{ .path = b.pathFromRoot("src/root.zig") },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);
    b.installArtifact(lib_wasm);

    const fixtures_unit_test = b.addTest(.{
        .root_source_file = .{ .path = b.pathFromRoot("tests/test_main.zig") },
        .target = target,
        .optimize = optimize,
    });

    const fixture_test_run = b.addRunArtifact(fixtures_unit_test);

    const lib_unit_test = b.addTest(.{
        .name = "ts-parser-zig",
        .root_source_file = .{ .path = b.pathFromRoot("src/root.zig") },
        .target = target,
        .optimize = optimize,
    });
    const lib_unit_test_run = b.addRunArtifact(lib_unit_test);

    const tsParserModule = b.createModule(.{ .root_source_file = .{ .path = b.pathFromRoot("src/root.zig") } });
    fixtures_unit_test.root_module.addImport("ts-parser-zig", tsParserModule);
    lib_unit_test.root_module.addImport("ts-parser-zig", tsParserModule);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&fixture_test_run.step);
    test_step.dependOn(&lib_unit_test_run.step);
}
