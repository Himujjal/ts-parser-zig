const std = @import("std");
const test_utils = @import("../../test_utils.zig");

const Allocator = std.Allocator;
const allocator = std.testing.allocator;
const print = std.debug.print;
const expect = std.testing.expect;

test "Expression Tests" {
    try testFile("primary/literal/numeric", "migrated_0000");
}

fn testFile(comptime folder: []const u8, comptime file: []const u8) !void {
	try test_utils.testFile("expression" ++ "/" ++ folder, file); 
}
