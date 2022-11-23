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
const TT = token.TokenType;

// ---------------- Nodes ------------------
const Program = nodes.Program;
const StmtListItem = nodes.StmtListItem;
const Block = nodes.Block;
const Stmt = nodes.Stmt;
const Raw = nodes.Raw;
const Decl = nodes.Decl;

const EmptyStmt = nodes.EmptyStmt;
const LabeledStmt = nodes.LabeledStmt;

const Expr = nodes.Expr;
const SeqExpr = nodes.SeqExpr;
const UnaryExpr = nodes.UnaryExpr;
const NewExpr = nodes.NewExpr;
const RegExp = nodes.RegExp;

const CodeLocation = token.CodeLocation;
// -----------------------------------------

const AllocPrintError = std.fmt.AllocPrintError;
const AllocatorError = Allocator.Error;

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

    pub fn render(r: *RendererJSON, program: Program, tokens: []const Token) ![]const u8 {
        var res: []const u8 = "{\"type\":\"Program\",\"body\":[";
        for (program.stmt_list_items) |stmt_list_item, i| {
            res = try r.concat(res, try r.renderStmtListItem(stmt_list_item));
            if (i != program.stmt_list_items.len - 1) {
                res = try r.concat(res, ",");
            }
        }
        res = try r.format("{s}],\"sourceType\":\"{s}\",\"tokens\":{s},\"range\":{s},\"loc\":{s}}}", .{
            res,
            switch (program.source_type) {
                .Module => "module",
                .Script => "script",
            },
            try r.renderTokens(tokens),
            try r.renderRange(program.loc),
            try r.renderLoc(program.loc),
        });
        return res;
    }

    pub fn renderTokens(r: *RendererJSON, tokens: []const Token) ![]const u8 {
        var res: []const u8 = "[";

        const length = if (tokens[tokens.len - 1].tok_type == TokenType.EOF) tokens.len - 1 else tokens.len;

        for (tokens) |tok, i| {
            if (tok.tok_type != TokenType.EOF and tok.tok_type != TokenType.WhitespaceToken and tok.tok_type != TokenType.CommentToken) {
                res = try r.concat(
                    res,
                    try r.format(
                        "{{\"type\":\"{s}\",\"value\":\"{s}\",\"range\":{s},\"loc\":{s}}}",
                        .{
                            tok.tok_type.toString(),
                            try r.renderToken(&tok),
                            try r.renderRange(&tok.loc),
                            try r.renderLoc(&tok.loc),
                        },
                    ),
                );
                if (i != length - 1) res = try r.concat(res, ",");
            }
        }
        res = try r.concat(res, "]");
        return res;
    }

    pub fn renderStmtListItem(r: *RendererJSON, stmt_list_item: StmtListItem) ![]const u8 {
        return switch (stmt_list_item) {
            .decl => |decl| try r.renderDecl(decl),
            .stmt => |stmt| try r.renderStmt(stmt),
            .raw => |raw| try r.renderRaw(raw),
        };
    }

    pub fn renderBlock(r: *RendererJSON, block: *Block) Allocator.Error![]const u8 {
        var res: []const u8 = "{\"type\":\"BlockStatement\",\"body\":[";
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
            .expr_stmt => |expr_stmt| {
                return try r.format(
                    "{{\"type\":\"ExpressionStatement\",\"expression\":{s},\"range\":{s},\"loc\":{s}}}",
                    .{ try r.renderExpr(expr_stmt.expr), try r.renderRange(expr_stmt.loc), try r.renderLoc(expr_stmt.loc) },
                );
            },
            .block_stmt => |block_stmt| try r.renderBlock(block_stmt),
            .labeled_stmt => |labeled_stmt| try r.renderLabeledStmt(labeled_stmt),
        };
    }

    pub fn renderLabeledStmt(r: *RendererJSON, labeled_stmt: *LabeledStmt) (AllocatorError || AllocPrintError)![]const u8 {
        var identifier = try r.format("{{\"type\":\"Identifier\",\"range\":{s}}}", .{try r.renderRange(
            &labeled_stmt.label.loc,
        )});
        var res: []const u8 = try r.format("{{ \"type\":\"LabeledStatement\",\"label\":{s},\"body\":{s},\"range\":{s},\"loc\":{s}}}", .{
            identifier,
            try r.renderStmt(labeled_stmt.body),
            try r.renderRange(labeled_stmt.loc),
            try r.renderLoc(labeled_stmt.loc),
        });
        return res;
    }

    pub fn renderEmptyStmt(r: *RendererJSON, stmt: EmptyStmt) ![]const u8 {
        return try r.format("{{\"type\":\"EmptyStatement\",\"range\":{s},\"loc\":{s}}}", .{
            try r.renderRange(stmt.loc),
            try r.renderLoc(stmt.loc),
        });
    }

    pub fn renderExpr(r: *RendererJSON, expr: Expr) ![]const u8 {
        return switch (expr) {
            .seq_expr => |s| r.renderSeqExpr(s),
            .unary_expr => |ue| r.renderUnaryExpr(ue),
            .new_expr => |ne| r.renderNewExpr(ne),

            .identifier => |id| r.renderIdentifier(id),
            .literal => |l| r.renderLiteral(l),
            .reg_exp => |reg_exp| r.renderRegExp(reg_exp),
        };
    }

    pub fn renderSeqExpr(r: *RendererJSON, s: *SeqExpr) ![]const u8 {
        var res: []const u8 = "{\"type\":\"SequenceExpression\",\"expressions\":[";

        for (s.exprs) |expr, i| {
            res = try r.concat(res, try r.renderExpr(expr));
            if (i != s.exprs.len - 1) res = try r.concat(res, ",");
        }
        res = try r.concat(
            res,
            try r.format("],\"range\":{s},\"loc\":{s}}}", .{ try r.renderRange(s.loc), try r.renderLoc(s.loc) }),
        );
        return res;
    }

    pub fn renderIdentifier(r: *RendererJSON, id: *Token) AllocPrintError![]const u8 {
        return try r.format(
            "{{\"type\":\"Identifier\",\"name\":\"{s}\",\"range\":{s},\"loc\":{s}}}",
            .{ try r.renderToken(id), try r.renderRange(&id.loc), try r.renderLoc(&id.loc) },
        );
    }

    pub fn renderLiteral(r: *RendererJSON, l: *Token) AllocPrintError![]const u8 {
        var value: []const u8 = try r.renderStrWithLoc(&l.loc);
        switch (l.tok_type) {
            TT.HexadecimalToken,
            TT.DecimalToken,
            => {
                var res: f64 = std.fmt.parseFloat(f64, value) catch 0;
                value = try r.format("{d}", .{res});
                return try r.format(
                    "{{\"type\":\"Literal\",\"value\":{s},\"raw\":\"{s}\",\"range\":{s},\"loc\":{s}}}",
                    .{ value, try r.renderToken(l), try r.renderRange(&l.loc), try r.renderLoc(&l.loc) },
                );
            },
            TT.BinaryToken,
            TT.OctalToken,
            TT.BigIntToken,
            => {},
            TT.OctalTokenWithoutO => {
                var res: u32 = std.fmt.parseInt(u32, value, 8) catch 0;
                value = try r.format("{d}", .{res});
                return try r.format(
                    "{{\"type\":\"Literal\",\"value\":{s},\"raw\":\"{s}\",\"range\":{s},\"loc\":{s}}}",
                    .{ value, try r.renderToken(l), try r.renderRange(&l.loc), try r.renderLoc(&l.loc) },
                );
            },
            TT.StringToken => {
                return try r.format(
                    "{{\"type\":\"Literal\",\"value\":{s},\"raw\":\"{s}\",\"range\":{s},\"loc\":{s}}}",
                    .{ value, try r.renderToken(l), try r.renderRange(&l.loc), try r.renderLoc(&l.loc) },
                );
            },
            else => {},
        }
        return value;
    }

    pub fn renderRegExp(r: *RendererJSON, reg_exp: *RegExp) AllocPrintError![]const u8 {
        return try r.format(
            "{{\"type\":\"Literal\",\"value\":{{}},\"raw\":\"/{s}/{s}\",\"regex\":{{\"pattern\":\"{s}\",\"flags\":\"{s}\"}},\"range\":{s},\"loc\":{s}}}",
            .{
                reg_exp.pattern,
                reg_exp.flags,
                reg_exp.pattern,
                reg_exp.flags,
                try r.renderRange(reg_exp.loc),
                try r.renderLoc(reg_exp.loc),
            },
        );
    }

    pub fn renderUnaryExpr(r: *RendererJSON, unary_expr: *UnaryExpr) AllocPrintError![]const u8 {
        return try r.format(
            "{{\"type\":\"UnaryExpression\",\"operator\":\"{s}\",\"argument\":\"{s}\",\"prefix\":true,\"range\":{s}}}",
            .{ try r.renderToken(unary_expr.op), try r.renderExpr(unary_expr.expr), try r.renderRange(unary_expr.loc) },
        );
    }

    pub fn renderNewExpr(r: *RendererJSON, new_expr: *NewExpr) AllocPrintError![]const u8 {
        return try r.format(
            "{{\"type\":\"NewExpression\",\"callee\":\"{s}\",\"range\":{s}}}",
            .{ try r.renderExpr(new_expr.expr), try r.renderRange(new_expr.loc) },
        );
    }

    pub fn renderToken(r: *RendererJSON, t: *const Token) ![]const u8 {
        const s = r.code[t.loc.start..t.loc.end];
        return switch (t.tok_type) {
            TT.StringToken => try r.renderRawString(s),
            else => s,
        };
    }

    // TODO
    pub fn renderRawString(r: *RendererJSON, s: []const u8) AllocatorError![]const u8 {
        var arr = ArrayList(u8).init(r._a);
        const quote = s[0];
        if (quote == '\'') {
            try arr.append('\'');
        } else if (quote == '"') {
            try arr.append('\\');
            try arr.append('"');
        }

		var i: usize = 1;
        while (i < s.len - 1): (i += 1) {
            try arr.append(s[i]);
        }

        if (quote == '\'') {
            try arr.append('\'');
        } else if (quote == '"') {
            try arr.append('\\');
            try arr.append('"');
        }
        return arr.items;
    }

    pub fn renderStrWithLoc(r: *RendererJSON, loc: *CodeLocation) ![]const u8 {
        return r.code[loc.start..loc.end];
    }

    pub fn renderRaw(r: *RendererJSON, raw: Raw) ![]const u8 {
        _ = raw;
        _ = r;
        return "";
    }

    pub fn renderDecl(r: *RendererJSON, decl: Decl) ![]const u8 {
        // TODO
        _ = decl;
        _ = r;
        return "";
    }

    pub fn concat(r: *RendererJSON, a: []const u8, b: []const u8) ![]const u8 {
        return try concatStrings(r._a, a, b);
    }

    pub fn renderLoc(r: *RendererJSON, loc: *const CodeLocation) AllocPrintError![]const u8 {
        return try r.format(
            "{{\"start\":{{\"line\":{d},\"column\":{d}}},\"end\":{{\"line\":{d},\"column\":{d}}}}}",
            .{ loc.start_line, loc.start_col, loc.end_line, loc.end_col },
        );
    }

    pub fn renderRange(r: *RendererJSON, loc: *const CodeLocation) ![]const u8 {
        return try r.format("[{d},{d}]", .{ loc.start, loc.end });
    }

    pub fn deinit(r: *RendererJSON) void {
        r.renderer_arena.deinit();
        r.allocator.destroy(r.renderer_arena);
    }

    pub fn format(r: *RendererJSON, comptime fmt: []const u8, args: anytype) std.fmt.AllocPrintError![]const u8 {
        return std.fmt.allocPrint(r._a, fmt, args);
    }
};
