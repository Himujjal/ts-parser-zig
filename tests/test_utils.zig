const std = @import("std");
const json5 = @import("json5.zig");
const parser = @import("../src/parser.zig");
const renderer_json = @import("../src/rendererJSON.zig");
const renderer = @import("../src/renderer.zig");
const Allocator = std.Allocator;
const allocator = std.testing.allocator;

const TSParser = parser.Parser;
const TSParserOptions = parser.ParserOptions;
const TSRendererJSON = renderer_json.RendererJSON;
const TSRenderer = renderer.Renderer;

const JSONParser = json5.Parser;
const JSONValue = json5.Value;

const print = std.debug.print;
const expect = std.testing.expect;

pub fn testFile(comptime folder: []const u8, comptime file_without_ext: []const u8) !void {
    const test_name = folder ++ "/" ++ file_without_ext;

    print("\n" ++ COLOR_LIGHT_VIOLET ++ "========= Running '{s}' Test ========\n" ++ COLOR_RESET, .{test_name});

    const source_str_json5 = @embedFile("fixtures/" ++ test_name ++ ".tree.json5");
    const source_str_ts = @embedFile("fixtures/" ++ test_name ++ ".ts");
    const js_str: []const u8 = @embedFile("fixtures/" ++ test_name ++ ".js");

    var json_parser: JSONParser = JSONParser.init(allocator, false);
    defer json_parser.deinit();
    var tree = try json_parser.parse(source_str_json5);
    defer tree.deinit();
    var ast_tree_json5 = try getStringifiedJSON(tree.root);
	defer ast_tree_json5.deinit();

    const options = TSParserOptions{};

    var p = TSParser.init(allocator, source_str_ts, options);
    defer p.deinit();
    const program = try p.parse();

    var r = try TSRendererJSON.init(allocator, source_str_json5);
    defer r.deinit();
    const rendered_json = try r.render(program);

    var json_parser_output = JSONParser.init(allocator, false);
	defer json_parser_output.deinit();
    var output_tree: json5.ValueTree = try json_parser_output.parse(rendered_json);
	defer output_tree.deinit();

    expect(try json5.json5Equal(allocator, output_tree.root, tree.root)) catch |err| {
        printError(ast_tree_json5.items, rendered_json);
        return err;
    };

    var output_renderer = try renderer.Renderer.init(allocator, source_str_ts);
	defer output_renderer.deinit();
	const rendered_js = try output_renderer.render(program);

	expect(std.mem.eql(u8, rendered_js, js_str)) catch |err| {
		printError(js_str, rendered_js);
		return err;
	};

    print(COLOR_LIGHT_VIOLET ++ "~~~~~~~~~ '{s}' Test SUCCESSFUL! ~~~~~~~~~~\n" ++ COLOR_RESET, .{test_name});
}

fn getOptions(sub_test: json5.ObjectMap) TSParserOptions {
    const options_key: ?JSONValue = sub_test.get("options");
    var options = TSParserOptions{};
    if (options_key) |ok| {
        _ = ok;
        // const options_json = ok.Object;
        // if (options_json.get("parseAtrulePrelude")) |val| options.parse_rule_prelude = val.Bool;
    }
    return options;
}

fn printError(expected: []const u8, output: []const u8) void {
    const ERROR = COLOR_RED ++ "\t\t!!ERROR!! " ++ COLOR_RESET;
    const EXPECTED = BACK_GREEN ++ "{s}" ++ COLOR_RESET;
    const GOT = BACK_RED ++ "{s}" ++ COLOR_RESET;
    print(ERROR ++ "EXPECTED: " ++ EXPECTED ++ " | GOT: " ++ GOT ++ "\n", .{ expected, output });
}

fn getStringifiedJSON(json5Value: JSONValue) !std.ArrayList(u8) {
    var arr = std.ArrayList(u8).init(allocator);
    try json5Value.json5Stringify(.{}, arr.writer());
    return arr;
}

const COLOR_RED = "\x1b[31m";
const COLOR_LIGHT_VIOLET = "\x1b[38;5;12m";
const COLOR_LIGHT_BLUE = "\x1b[38;5;159m";
const COLOR_YELLOW = "\x1b[38;5;214m";
const COLOR_GREEN = "\x1b[38;5;40m";
const COLOR_RESET = "\x1b[0m";
const BACK_RED = "\x1b[48;5;124m";
const BACK_GREEN = "\x1b[48;5;28m";
