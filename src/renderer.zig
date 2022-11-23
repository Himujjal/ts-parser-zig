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
const TT = token.TokenType;

// ---------------- Nodes ------------------
const Program = nodes.Program;
const Block = nodes.Block;
const Stmt = nodes.Stmt;
const Expr = nodes.Expr;
const SeqExpr = nodes.SeqExpr;
const Raw = nodes.Raw;
const Decl = nodes.Decl;
const LabeledStmt = nodes.LabeledStmt;

const UnaryExpr = nodes.UnaryExpr;
const NewExpr = nodes.NewExpr;
const RegExp = nodes.RegExp;

const StmtListItem = nodes.StmtListItem;
const CodeLocation = token.CodeLocation;
// -----------------------------------------

const Err = Allocator.Error;

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

    pub fn render(r: *Renderer, program: Program) ![]const u8 {
        var res: []const u8 = "";
        for (program.stmt_list_items) |stmt_list_item| {
            res = try r.concat(res, try r.renderStmtListItem(stmt_list_item));
        }
        return res;
    }

    pub fn renderStmtListItem(r: *Renderer, stmt_list_item: StmtListItem) ![]const u8 {
        return switch (stmt_list_item) {
            .decl => |decl| try r.renderDecl(decl),
            .stmt => |stmt| try r.renderStmt(stmt),
            .raw => |raw| try r.renderRaw(raw),
        };
    }

    pub fn renderRaw(r: *Renderer, raw: Raw) ![]const u8 {
        if (raw.tokens.len == 0) return "";
        const first = raw.tokens[0];
        const last = raw.tokens[raw.tokens.len - 1];
        return r.code[first.loc.start..last.loc.end];
    }

    pub fn renderDecl(r: *Renderer, decl: Decl) ![]const u8 {
        // TODO: implement it later
        _ = r;
        _ = decl;
        return "";
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

    pub fn renderLabeledStmt(r: *Renderer, labeled_stmt: *LabeledStmt) ![]const u8 {
        return try r.format("{s}:{s}", .{ r.renderToken(labeled_stmt.label), try r.renderStmt(labeled_stmt.body) });
    }

    pub fn renderStmt(r: *Renderer, stmt: Stmt) Err![]const u8 {
        return switch (stmt) {
            .empty_stmt => ";",
            .expr_stmt => |expr_stmt| try r.concat(try r.renderExpr(expr_stmt.expr), ";"),
            .block_stmt => |block_stmt| try r.renderBlock(block_stmt),
            .labeled_stmt => |labeled_stmt| try r.renderLabeledStmt(labeled_stmt),
        };
    }

    pub fn renderExpr(r: *Renderer, expr: Expr) Err![]const u8 {
        return switch (expr) {
            .seq_expr => |seq_expr| r.renderSeqExpr(seq_expr),
            .unary_expr => |unary_expr| r.renderUnaryExpr(unary_expr),
            .new_expr => |new_expr| r.renderNewExpr(new_expr),

            .identifier => |identifier| r.renderToken(identifier.*),
            .literal => |literal| r.renderLiteral(literal.*),
            .reg_exp => |reg_exp| r.renderRegExp(reg_exp),
        };
    }

    pub fn renderRegExp(r: *Renderer, reg_exp: *RegExp) Err![]const u8 {
		return try r.format("/{s}/{s}", .{ reg_exp.pattern, reg_exp.flags });
    }

    pub fn renderLiteral(r: *Renderer, tok: Token) Err![]const u8 {
        var value: []const u8 = r.renderToken(tok);
        switch (tok.tok_type) {
            TT.HexadecimalToken,
            TT.DecimalToken,
            => {},
            TT.BinaryToken,
            TT.OctalToken,
            TT.BigIntToken,
            => {},
            TT.OctalTokenWithoutO => {
                var res: u32 = std.fmt.parseInt(u32, value, 8) catch |e| {
                    std.debug.print("{}\n", .{e});
                    return "0";
                };
                value = try r.format("{d}", .{res});
            },
            else => {},
        }
        return value;
    }

    pub fn renderSeqExpr(r: *Renderer, seq_expr: *SeqExpr) Err![]const u8 {
        var res: []const u8 = "";
        for (seq_expr.exprs) |expr, i| {
            res = try r.concat(res, try r.renderExpr(expr));
            if (i != seq_expr.exprs.len - 1) res = try r.concat(res, ",");
        }
        return res;
    }

    pub fn renderUnaryExpr(r: *Renderer, unary_expr: *UnaryExpr) Err![]const u8 {
        return try r.format("{s}{s}", .{ r.renderToken(unary_expr.op.*), try r.renderExpr(unary_expr.expr) });
    }

    pub fn renderNewExpr(r: *Renderer, new_expr: *NewExpr) Err![]const u8 {
        return try r.format("new {s}", .{try r.renderExpr(new_expr.expr)});
    }

    pub fn renderToken(r: *Renderer, t: Token) []const u8 {
        return r.code[t.loc.start..t.loc.end];
    }

    pub fn concat(r: *Renderer, a: []const u8, b: []const u8) Err![]const u8 {
        return concatStrings(r._a, a, b);
    }

    pub fn format(r: *Renderer, comptime fmt: []const u8, args: anytype) Err![]const u8 {
        return std.fmt.allocPrint(r._a, fmt, args);
    }

    pub fn deinit(r: *Renderer) void {
        r.renderer_arena.deinit();
        r.allocator.destroy(r.renderer_arena);
    }
};
