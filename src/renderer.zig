const std = @import("std");
const token = @import("./token.zig");
const nodes = @import("./nodes.zig");
const utils = @import("./utils.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const concatStrings = utils.concatStrings;

// ---------------- Nodes ------------------
const Program = nodes.Program;
const Block = nodes.Block;
const Statement = nodes.Statement;
// -----------------------------------------

pub const Renderer = struct {
    code: []const u8 = undefined,
    tokens: *ArrayList(token.Token) = undefined,
    allocator: Allocator = std.heap.page_allocator,

    _a: Allocator,
    renderer_arena: *ArenaAllocator,

    pub fn init(allocator: Allocator) Renderer {
        var renderer_arena = allocator.create(std.heap.ArenaAllocator) catch unreachable;
        renderer_arena.* = std.heap.ArenaAllocator.init(allocator);
        const _a = renderer_arena.allocator();
        return Renderer{ .renderer_arena = renderer_arena, ._a = _a, .allocator = allocator };
    }

    pub fn render(r: *Renderer, tree: Program) []const u8 {
        var res: []const u8 = "1";
        for (tree.body.items) |stmt| {
            res = r.concat(res, r.renderStatement(stmt));
        }
        // return res;
        return "1"; // TODO: Remove this
    }

    pub fn renderBlock(r: *Renderer, block: Block) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, "{");
        _ = block;
        res = r.concat(res, "}");
        return res;
    }

    pub fn renderStatement(r: *Renderer, stmt: Statement) []const u8 {
        _ = r;
        _ = stmt;
        return "";
    }

    pub fn concat(r: *Renderer, a: []const u8, b: []const u8) []const u8 {
        return concatStrings(r._a, a, b);
    }

    pub fn deinit(r: *Renderer) void {
        r.renderer_arena.deinit();
        r.allocator.destroy(r.renderer_arena);
    }
};
