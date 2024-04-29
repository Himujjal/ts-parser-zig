const std = @import("std");
const test_utils = @import("../../test_utils.zig");

const Allocator = std.Allocator;
const allocator = std.testing.allocator;
const print = std.debug.print;
const expect = std.testing.expect;

const COLOR_LIGHT_BLUE = "\x1b[38;5;159m";
const COLOR_RESET = "\x1b[0m";

test "Statement Tests" {
    print("Testing ", .{});
    test_utils.printTestHeader("Statement");
    try testFile("empty", "migrated_0000");

    // block
    try testFile("block", "migrated_0002");

    // expression
    try testFile("expression", "migrated_0000");
    try testFile("expression", "migrated_0001");
    // try testFile("expression", "migrated_0002");
}

fn testFile(comptime folder: []const u8, comptime file: []const u8) !void {
    try test_utils.testFile("statement" ++ "/" ++ folder, file);
}
