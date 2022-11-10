const std = @import("std");
const token = @import("token.zig");
const nodes = @import("nodes.zig");
const utils = @import("utils.zig");

const allocPrint = std.fmt.allocPrint;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const concatStrings = utils.concatStrings;

const Token = token.Token;
const TokenType = token.TokenType;

// ---------------- Nodes ------------------
const Program = nodes.Program;
const Block = nodes.Block;
const Stmt = nodes.Stmt;

const EmptyStmt = nodes.EmptyStmt;

const Expr = nodes.Expr;
const CodeLocation = token.CodeLocation;
// -----------------------------------------

pub const RendererJSON = struct {
    code: []const u8 = undefined,
    allocator: Allocator = std.heap.page_allocator,

    _a: Allocator,
    renderer_arena: *ArenaAllocator,

    pub fn init(allocator: Allocator, code: []const u8) !RendererJSON {
        var renderer_arena = try allocator.create(std.heap.ArenaAllocator);
        renderer_arena.* = std.heap.ArenaAllocator.init(allocator);
        const _a = renderer_arena.allocator();
        return RendererJSON{
            .renderer_arena = renderer_arena,
            ._a = _a,
            .allocator = allocator,
            .code = code,
        };
    }

    pub fn render(r: *RendererJSON, tree: Program) ![]const u8 {
        var res: []const u8 = "{\"type\":\"Program\",\"body\":[";
        for (tree.stmts) |stmt, i| {
            res = try r.format("{s}{s}", .{ res, try r.renderStmt(stmt) });
            if (i != tree.stmts.len - 1) {
                res = try r.concat(res, ",");
            }
        }
        res = try r.format("{s}],\"loc\":{s},\"range\":{s},\"sourceType\":\"{s}\"}}", .{
            res,
            try r.renderLoc(tree.loc),
            try r.renderRange(tree.loc),
            switch (tree.source_type) {
                .Module => "module",
                .Script => "script",
            },
        });
        return res;
    }

    pub fn renderBlock(r: *RendererJSON, block: *Block) Allocator.Error![]const u8 {
        var res: []const u8 = "{\"type\":\"BlockStatement\",body:[";
        for (block.stmts) |stmt, i| {
            res = try r.concat(res, try r.renderStmt(stmt));
            if (i != block.stmts.len - 1) res = try r.concat(res, ",");
        }
        return try r.format("{s}],\"range\":{s},\"loc\":{s}}}", .{
            res,
            try r.renderRange(block.loc),
            try r.renderLoc(block.loc),
        });
    }

    pub fn renderStmt(r: *RendererJSON, stmt: Stmt) ![]const u8 {
        return switch (stmt) {
            .empty_stmt => |empty_stmt| try r.renderEmptyStmt(empty_stmt.*),
            .expr_stmt => |expr_stmt| try r.renderExpr(expr_stmt.expr),
            .block_stmt => |block_stmt| try r.renderBlock(block_stmt),
        };
    }

    pub fn renderEmptyStmt(r: *RendererJSON, stmt: EmptyStmt) ![]const u8 {
        return try r.format("{{\"type\":\"EmptyStatement\",\"range\":{s},\"loc\":{s}}}", .{
            try r.renderRange(stmt.loc),
            try r.renderLoc(stmt.loc),
        });
    }

    pub fn renderExpr(r: *RendererJSON, expr: Expr) ![]const u8 {
        return switch (expr) {
            .literal => |literal| r.renderToken(literal),
        };
    }

    pub fn renderToken(r: *RendererJSON, t: *Token) []const u8 {
        return r.code[t.start..t.end];
    }

    pub fn concat(r: *RendererJSON, a: []const u8, b: []const u8) ![]const u8 {
        return try concatStrings(r._a, a, b);
    }

    pub fn renderLoc(r: *RendererJSON, loc: *CodeLocation) ![]const u8 {
        return try r.format(
            "{{\"start\":{{\"line\":{d},\"column\":{d}}},\"end\":{{\"line\":{d},\"column\":{d}}}}}",
            .{ loc.start_line, loc.start_col, loc.end_line, loc.end_col },
        );
    }

    pub fn renderRange(r: *RendererJSON, loc: *CodeLocation) ![]const u8 {
        return try r.format("[{d},{d}]", .{ loc.start, loc.end });
    }

    pub fn deinit(r: *RendererJSON) void {
        r.renderer_arena.deinit();
        r.allocator.destroy(r.renderer_arena);
    }

    pub fn format(r: *RendererJSON, comptime fmt: []const u8, args: anytype) ![]const u8 {
        return std.fmt.allocPrint(r._a, fmt, args);
    }
};
