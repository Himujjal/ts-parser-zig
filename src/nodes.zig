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
    source_type: enum { Module, Script } = .Module,
    stmt_list_item: []const StmtListItem = undefined,
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

pub const Decl = union(enum) {};

pub const Block = struct {
    loc: *CodeLocation = undefined,
    stmts: []const Stmt = undefined,
};

pub const Stmt = union(enum) {
    expr_stmt: *ExprStmt,
    empty_stmt: *EmptyStmt,
    block_stmt: *Block,
};

pub const EmptyStmt = struct { loc: *CodeLocation = undefined };

pub const ExprStmt = struct {
    loc: *CodeLocation = undefined,
    expr: *Expr,
};

pub const Expr = union(enum) {
    literal: Token,
};
