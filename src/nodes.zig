const std = @import("std");
const _error = @import("error.zig");
const token = @import("token.zig");

const Allocator = std.mem.Allocator;

const Token = token.Token;
const TokenType = token.TokenType;
const CodeLocation = token.CodeLocation;

const ArrayList = std.ArrayList;

const default_loc = CodeLocation{};

fn allocLoc(a: Allocator, loc: CodeLocation) !*CodeLocation {
    const l = try a.create(loc);
    l.* = loc;
    return l;
}

pub const Program = struct {
    loc: *CodeLocation = undefined,
    source_type: enum { Module, Script } = .Script,
    stmt_list_items: []const StmtListItem = undefined,
};

pub const StmtListItem = union(enum) {
    decl: Decl,
    stmt: Stmt,
    raw: Raw,
};

pub const Raw = struct {
    loc: *CodeLocation = undefined,
    tokens: []const *Token,
};

pub const Decl = union(enum) {
    async_func: AsyncFuncDecl,
};

pub const AsyncFuncDecl = struct {
    loc: *CodeLocation = undefined,
};

pub const Block = struct {
    loc: *CodeLocation = undefined,
    stmts: []const Stmt = undefined,
};

pub const Stmt = union(enum) {
    expr_stmt: *ExprStmt,
    empty_stmt: *EmptyStmt,
    block_stmt: *Block,
    labeled_stmt: *LabeledStmt,
};

pub const EmptyStmt = struct { loc: *CodeLocation = undefined };

pub const ExprStmt = struct {
    loc: *CodeLocation = undefined,
    expr: Expr = undefined,
};

pub const LabeledStmt = struct {
    loc: *CodeLocation = undefined,
	label: Token = undefined, // Identifier
	body: Stmt = undefined,
};

pub const Expr = union(enum) {
	seq_expr: *SeqExpr,
	unary_expr: *UnaryExpr,
	new_expr: *NewExpr,

    identifier: *Token,
	literal: *Token,
	reg_exp: *RegExp,
};

pub const UnaryExpr = struct {
	loc: *CodeLocation = undefined,
	op: *Token,
	expr: Expr,
};

pub const SeqExpr = struct {
	loc: *CodeLocation = undefined,
	exprs: []const Expr,
};

pub const NewExpr = struct {
	loc: *CodeLocation = undefined,
	expr: Expr,
};

pub const RegExp = struct {
	loc: *CodeLocation = undefined,
	pattern: []const u8 = undefined, 
	flags: []const u8 = undefined,
};
