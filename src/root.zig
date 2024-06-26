const std = @import("std");
const expect = std.testing.expect;
pub const parser = @import("parser.zig");
pub const renderer_json = @import("rendererJSON.zig");
pub const renderer = @import("renderer.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const ParserOptions = parser.ParserOptions;
pub const Parser = parser.Parser;
pub const Error = parser.Error;
pub const ParserErrorType = parser.ParserErrorType;

pub fn parseText(allocator: Allocator, text: []const u8, options: ParserOptions) Parser {
    var _parser = Parser.init(allocator, options);
    _ = (&_parser).parse(text);
    return _parser;
}

test "Parse Test" {
    _ = @import("scanner_test.zig");
    // _ = @import("parser_test.zig");
}
