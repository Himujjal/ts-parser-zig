const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const lib = b.addStaticLibrary("ts-parser-zig", "src/lib.zig");
    lib.setBuildMode(mode);
    lib.install();

    const lib2 = b.addSharedLibrary("ts-parser-zig", "src/lib.zig", .unversioned);
    lib2.setBuildMode(mode);
    lib2.setTarget(.{ .cpu_arch = .wasm32, .os_tag = .wasi });
    lib2.install();

    const test_step = b.step("test", "Run library tests");

    const main_tests = b.addTest("src/lib.zig");
    main_tests.setBuildMode(mode);
    test_step.dependOn(&main_tests.step);

    // const json_tests = b.addTest("tests/test_main.zig");
    // json_tests.main_pkg_path = ".";
    // json_tests.addIncludePath("src");
    // json_tests.setBuildMode(mode);
    // test_step.dependOn(&json_tests.step);
}
