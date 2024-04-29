const std = @import("std");
const json5 = @import("json5.zig");
const tsParserZig = @import("ts-parser-zig");
const parser = tsParserZig.parser;
const renderer_json = tsParserZig.renderer_json;
const renderer = tsParserZig.renderer;

// const parser = @import("src/parser.zig");
// const renderer_json = @import("src/rendererJSON.zig");
// const renderer = @import("src/renderer.zig");
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

/// The following steps take place:
/// 1. Parse
/// 2. Render
/// 3. Parse rendered JSON
/// 4. Compare parsed JSON with expected JSON
/// 5. Render to JS
/// 6. Compare rendered JS with expected JS
pub fn testFile(comptime folder: []const u8, comptime file_without_ext: []const u8) !void {
    const test_name = folder ++ "/" ++ file_without_ext;

    print("\n\t" ++ COLOR_LIGHT_VIOLET ++ "========= Running '{s}' Test ========\n" ++ COLOR_RESET, .{test_name});

    const expected_tree_json5 = @embedFile("fixtures/" ++ test_name ++ ".tree.json5");
    const source_str_ts = @embedFile("fixtures/" ++ test_name ++ ".ts");
    const js_str: []const u8 = @embedFile("fixtures/" ++ test_name ++ ".js");

    var expected_json_parser: JSONParser = JSONParser.init(allocator, false);
    defer expected_json_parser.deinit();
    var tree = expected_json_parser.parse(expected_tree_json5) catch |json_err| {
        std.debug.print("1. JSON Parse Error: {}\n, File: {s}\n", .{ json_err, test_name ++ ".tree.json5" });
        return json_err;
    };
    defer tree.deinit();
    var expected_tree_json5_string = try getStringifiedJSON(tree.root);
    defer expected_tree_json5_string.deinit();

    const options = TSParserOptions{};

    var p = TSParser.init(allocator, source_str_ts, options);
    defer p.deinit();
    const program = try p.parse();

    var r = try TSRendererJSON.init(allocator, source_str_ts);
    defer r.deinit();

    const rendered_json = try r.render(program, p.tokens.items);

    // std.debug.print("\n{s}\n\n{s}\n", .{rendered_json, expected_tree_json5_string.items});

    var json_parser_output = JSONParser.init(allocator, false);
    defer json_parser_output.deinit();
    var output_tree: json5.ValueTree = json_parser_output.parse(rendered_json) catch |json_err| {
        std.debug.print("2. JSON Parse Error: {}\n{s}\n", .{ json_err, rendered_json });
        return json_err;
    };
    defer output_tree.deinit();

    expect(try json5.json5Equal(allocator, output_tree.root, tree.root)) catch |err| {
        printError(expected_tree_json5_string.items, rendered_json);
        return err;
    };

    var output_renderer = try renderer.Renderer.init(allocator, source_str_ts);
    defer output_renderer.deinit();
    const rendered_js = try output_renderer.render(program);

    expect(std.mem.eql(u8, rendered_js, js_str)) catch |err| {
        printError(js_str, rendered_js);
        return err;
    };

    print(COLOR_LIGHT_VIOLET ++ "\t~~~~~~~~~ '{s}' Test SUCCESSFUL! ~~~~~~~~~~\n" ++ COLOR_RESET, .{test_name});
}

fn getOptions(sub_test: json5.ObjectMap) TSParserOptions {
    const options_key: ?JSONValue = sub_test.get("options");
    const options = TSParserOptions{};
    if (options_key) |ok| {
        _ = ok;
        // const options_json = ok.Object;
        // if (options_json.get("parseAtrulePrelude")) |val| options.parse_rule_prelude = val.Bool;
    }
    return options;
}

fn getJSONFromJSON5(json5_str: []const u8) []const u8 {
    var expected_json_parser: JSONParser = JSONParser.init(allocator, false);
    defer expected_json_parser.deinit();
    var tree = try expected_json_parser.parse(json5_str);
    defer tree.deinit();
    var expected_tree_json5_string = try getStringifiedJSON(tree.root);
    defer expected_tree_json5_string.deinit();

    const res: []const u8 = allocator.alloc(u8, expected_tree_json5_string.items.len);
    std.mem.copy(u8, res, expected_tree_json5_string.items);
    return res;
}

fn printError(expected: []const u8, output: []const u8) void {
    const ERROR = COLOR_RED ++ "\t\t!!ERROR!!\n" ++ COLOR_RESET;
    const EXPECTED = BACK_GREEN ++ "{s}" ++ COLOR_RESET;
    const GOT = BACK_RED ++ "{s}" ++ COLOR_RESET;
    print(ERROR ++ "EXPECTED:\n" ++ EXPECTED ++ "\nGOT:\n" ++ GOT ++ "\n", .{ expected, output });
}

fn getStringifiedJSON(json5Value: JSONValue) !std.ArrayList(u8) {
    var arr = std.ArrayList(u8).init(allocator);
    try json5Value.json5Stringify(.{}, arr.writer());
    return arr;
}

pub fn printTestHeader(comptime str: []const u8) void {
    std.debug.print("\n" ++ COLOR_LIGHT_BLUE ++ "======= '{s}' tests ========" ++ COLOR_RESET, .{str});
}

const COLOR_RED = "\x1b[31m";
const COLOR_LIGHT_VIOLET = "\x1b[38;5;12m";
const COLOR_LIGHT_BLUE = "\x1b[38;5;159m";
const COLOR_YELLOW = "\x1b[38;5;214m";
const COLOR_GREEN = "\x1b[38;5;40m";
const COLOR_RESET = "\x1b[0m";
const BACK_RED = "\x1b[48;5;124m";
const BACK_GREEN = "\x1b[48;5;28m";
