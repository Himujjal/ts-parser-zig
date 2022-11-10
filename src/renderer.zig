/// Converts TypeScript/JavaScript to JavaScript
const std = @import("std");
const token = @import("token.zig");
const nodes = @import("nodes.zig");
const utils = @import("utils.zig");

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
const Expr = nodes.Expr;
const CodeLocation = token.CodeLocation;
// -----------------------------------------

pub const Renderer = struct {
    code: []const u8 = undefined,
    allocator: Allocator = std.heap.page_allocator,

    _a: Allocator,
    renderer_arena: *ArenaAllocator,

    pub fn init(allocator: Allocator, code: []const u8) !Renderer {
        var renderer_arena = try allocator.create(std.heap.ArenaAllocator);
        renderer_arena.* = std.heap.ArenaAllocator.init(allocator);
        const _a = renderer_arena.allocator();
        return Renderer{
            .renderer_arena = renderer_arena,
            ._a = _a,
            .allocator = allocator,
            .code = code,
        };
    }

    pub fn render(r: *Renderer, tree: Program) ![]const u8 {
        var res: []const u8 = "";
        for (tree.stmts) |stmt| {
            res = try r.concat(res, try r.renderStmt(stmt));
        }
        return res;
    }

    pub fn renderBlock(r: *Renderer, block: *Block) ![]const u8 {
        var res: []const u8 = "";
        res = try r.concat(res, "{");
		for (block.stmts) |stmt| {
			res = try r.concat(res, try r.renderStmt(stmt));
		}
        res = try r.concat(res, "}");
        return res;
    }

    pub fn renderStmt(r: *Renderer, stmt: Stmt) Allocator.Error![]const u8 {
        return switch (stmt) {
            .empty_stmt => ";",
            .expr_stmt => |expr_stmt| try r.concat(try r.renderExpr(expr_stmt.expr), ";"),
			.block_stmt => |block_stmt| try r.renderBlock(block_stmt),
        };
    }

    pub fn renderExpr(r: *Renderer, expr: Expr) Allocator.Error![]const u8 {
        return switch (expr) {
            .literal => |literal| r.renderToken(literal),
        };
    }

    pub fn renderToken(r: *Renderer, t: *Token) []const u8 {
        return r.code[t.start..t.end];
    }

    pub fn concat(r: *Renderer, a: []const u8, b: []const u8) Allocator.Error![]const u8 {
        return concatStrings(r._a, a, b);
    }

    pub fn deinit(r: *Renderer) void {
        r.renderer_arena.deinit();
        r.allocator.destroy(r.renderer_arena);
    }
};
