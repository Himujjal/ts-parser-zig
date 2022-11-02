const std = @import("std");
const _error = @import("error.zig");
const token = @import("token.zig");

const Allocator = std.mem.Allocator;

const Token = token.TokenType;
const TokenType = token.TokenType;
const CodeLocation = token.CodeLocation;

const ArrayList = std.ArrayList;

const default_loc = CodeLocation{};

fn allocLoc(a: Allocator, loc: CodeLocation) !*CodeLocation {
    const l = try a.create(loc);
    l.* = loc;
    return l;
}

const Tree = struct {
    loc: *CodeLocation = undefined,
    block: *BlockBody = undefined,
};

const BlockBody = struct {
    loc: *CodeLocation = undefined,
    stmts: []const Statement = undefined,
};

const Statement = union(enum) {
    expr_stmt: *ExpressionStmt,
};

const ExpressionStmt = struct {
    loc: *CodeLocation = undefined,
    expr: Expr,
};

const Expr = union(enum) {
	
};
