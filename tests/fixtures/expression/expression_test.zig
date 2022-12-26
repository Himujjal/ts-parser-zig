const std = @import("std");
const test_utils = @import("../../test_utils.zig");

const Allocator = std.Allocator;
const allocator = std.testing.allocator;
const print = std.debug.print;
const expect = std.testing.expect;

fn getNum(comptime j: usize) ![]const u8 {
    var buf: [3]u8 = undefined;
    return try std.fmt.bufPrint(&buf, if (j < 10) "0{d}" else "{d}", .{j});
}

test "Expression Tests" {
    test_utils.printTestHeader("Expression > primary > literal > numeric");

    comptime var i: usize = 0;
    inline while (i < 25) : (i += 1) {
        const num = try comptime getNum(i);
        try testFile("primary/literal/numeric", "migrated_00" ++ num);
    }

    test_utils.printTestHeader("Expression > primary > literal > regular-expression");
    // TODO: Test everything later
    comptime var j: usize = 0;
    inline while (j < 1) : (j += 1) {
        const num = try comptime getNum(j);
        try testFile("primary/literal/regular-expression", "migrated_00" ++ num);
    }

    // test_utils.printTestHeader("Expression > primary > literal > string");
    // // TODO: Test everything later
    // comptime var k: usize = 0;
    // inline while (k < 1) : (k += 1) {
    //     if (k == 4 or k == 5 or k == 14) continue;
    //     const num = try comptime getNum(j);
    //     try testFile("primary/literal/string", "migrated_00" ++ num);
    // }
}

test "Expression > Primary > Array" {
    test_utils.printTestHeader("Expression > primary > literal > array");
    comptime var i: usize = 0;
    inline while (i < 13) : (i += 1) {
		const num = try comptime getNum(i);
        try testFile("primary/array", "migrated_00" ++ num);
	}
}

fn testFile(comptime folder: []const u8, comptime file: []const u8) !void {
    try test_utils.testFile("expression" ++ "/" ++ folder, file);
}
