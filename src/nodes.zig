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
    async_func: *AsyncFuncDecl,
    var_decl: *VarDecl,
};

pub const VarDecl = struct {
    loc: *CodeLocation = undefined,
    decls: []const VarDeclarator = undefined,
    kind: VarDeclKind = .Var,
};

pub const VarDeclKind = enum { Var, Const, Let };

pub const VarDeclarator = struct {
    loc: *CodeLocation = undefined,
    id: BindingPatternIdentifier = undefined,
	init: ?Expr = null,
};

pub const BindingPatternIdentifier = union(enum) {
    binding_identifier: BindingIdentifier,
    binding_pattern: BindingPattern,
};

pub const BindingIdentifier = Token;

pub const BindingPattern = union(enum) {
    array_pattern: *ArrayPattern,
    object_pattern: *ObjectPattern,
};

pub const ArrayPattern = struct {
    loc: *CodeLocation = undefined,
    elements: []const ?ArrayPatternElement,
};

pub const ArrayPatternElement = union(enum) {
    assignment_pattern: *AssignmentPattern,
    binding_identifier: BindingIdentifier,
	binding_pattern: BindingPattern,
	rest_element: *RestElement,
};

pub const ObjectPattern = struct {
    loc: *CodeLocation = undefined,
    properties: []const ObjectPatternProperty,
};

pub const ObjectPatternProperty = union(enum) {
    property: *Property,
    rest_element: *RestElement,
};

pub const Property = struct {
    loc: *CodeLocation = undefined,
    key: *PropertyKey = undefined,
    computed: bool = false,
    value: ?PropertyValue = null,
    // kind: []const u8 = undefined, // TODO: Look into this later
    method: bool = false,
    shorthand: bool = false,
};

pub const PropertyKey = union(enum) {
    identifier: *Token,
    literal: *Token,
};

pub const PropertyValue = union(enum) {
    assignment_pattern: *AssignmentPattern,
    // async_func_expr: *AsyncFunctionExpression, // TODO: Implement this later
    binding_identifier: BindingIdentifier,
    binding_pattern: BindingPattern,
    // func_expr: *FunctionExpression, // TODO: Implement this later
};

pub const PatternWithDefault = union(enum) {
    assignment_pattern: *AssignmentPattern,
    binding_identifier: BindingIdentifier,
    binding_pattern: BindingPattern,
};

pub const AssignmentPattern = struct {
    loc: *CodeLocation = undefined,
    left: BindingPatternIdentifier = undefined,
    right: Expr = undefined,
};

pub const RestElement = struct {
    loc: *CodeLocation = undefined,
    arg: BindingPatternIdentifier,
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
    var_stmt: *VarDecl,
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

    assignment_expr: *AssignmentExpr,
    arr_expr: *ArrayExpr,
    arrow_func: *ArrowFunc,
};

pub const ArrayExpr = struct {
    loc: *CodeLocation,
    elements: []const ?ArrayExprElement,
};

pub const AssignmentExpr = struct {
    loc: *CodeLocation,
    left_expr: Expr,
    right_expr: Expr,
};

pub const ArrowFunc = struct {
    loc: *CodeLocation = undefined,
    args: Expr = undefined,
    body: *ArrowFuncBody = undefined,
    generator: bool = false,
    expression: bool = false,
    _async: bool = false,
};

pub const ArrowFuncBody = union(enum) {
    block: *Block,
    expr: Expr,
};

pub const UnaryExpr = struct {
    loc: *CodeLocation = undefined,
    op: *Token,
    expr: Expr,
};

pub const SeqExpr = struct {
    loc: *CodeLocation = undefined,
    exprs: []const ?Expr,
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

pub const ArrayExprElement = union(enum) {
    expr: Expr,
    spread_element: *SpreadElement,
};

pub const SpreadElement = struct {
    loc: *CodeLocation = undefined,
    arg: Expr,
};
