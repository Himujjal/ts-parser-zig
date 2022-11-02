const std = @import("std");

const _parser = @import("parser.zig");
const scanner = @import("scanner.zig");
const _error = @import("error.zig");
const token = @import("token.zig");
const utils = @import("utils.zig");
const renderer = @import("renderer.zig");

const Token = token.Token;

const Renderer = renderer.Renderer;
const Parser = _parser.Parser;
const ParserOptions = _parser.ParserOptions;

const expect = std.testing.expect;
const _a = std.testing.allocator;

const parser_tests = [_][]const u8{
    "1",
};

test "Parser Tests" {
    const MAX = 1;
    for (parser_tests) |parser_test, i| {
        if (MAX == i) {}

        var p = Parser.init(_a, parser_test, ParserOptions{});
        defer p.deinit();

        _ = p.parse();

        var r = Renderer.init(_a);
        defer r.deinit();

        const output = r.render(p.tree);

        expect(std.mem.eql(u8, parser_test, output)) catch |err| {
            std.debug.print("Code not matching ({d}): INPUT: {s} | OUTPUT: {s}\n", .{ i, parser_test, output });
            return err;
        };
    }
}

test "Automatic Semicolon" {}
