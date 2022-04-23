const svelte_parser = @import("./lib.zig");
const builtin = @import("builtin");

/// Parser Options for parsing a svelte string
pub const ParserOptions = struct {
    filename: []const u8 = "",
};
