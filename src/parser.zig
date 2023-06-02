const std = @import("std");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const nodes = @import("nodes.zig");
const _error = @import("error.zig");
const utils = @import("utils.zig");

const eqStr = utils.eqStr;
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Scanner = scanner.Scanner;

const Location = token.CodeLocation;
const Token = token.Token;
const TokenType = token.TokenType;
const ContextualKeyword = token.ContextualKeyword;
const TT = TokenType;

const Error = _error.ParserError;
const ParserErrorType = _error.ParserErrorType;

pub const ParserOptions = struct {
    is_jsx_enabled: bool = true,
    is_ts_enabled: bool = false,
    strict: bool = true,
    is_module: bool = true,
    allow_yield: bool = true,
};

const Err = std.mem.Allocator.Error;

// ------ Nodes --------
const Program = nodes.Program;
const Stmt = nodes.Stmt;
const EmptyStmt = nodes.EmptyStmt;
const Block = nodes.Block;
const ExprStmt = nodes.ExprStmt;
const Raw = nodes.Raw;
const LabeledStmt = nodes.LabeledStmt;
// const DirectivePrologue = nodes.DirectivePrologue;
// const WSs = nodes.WSs;

// const ExportStmt = nodes.ExportStmt;
// const ImportStmt = nodes.ImportStmt;
const VarDecl = nodes.VarDecl;
// const FunctionDecl = nodes.FunctionDecl;
// const ClassDecl = nodes.ClassDecl;

// const BreakStmt = nodes.BreakStmt;
// const ContinueStmt = nodes.ContinueStmt;
// const DebuggerStmt = nodes.DebuggerStmt;
// const DoStmt = nodes.DoStmt;
// const ForStmt = nodes.ForStmt;
// const FunctionStmt = nodes.FunctionStmt;
// const IfStmt = nodes.IfStmt;
// const ReturnStmt = nodes.ReturnStmt;
// const SwitchStmt = nodes.SwitchStmt;
// const ThrowStmt = nodes.ThrowStmt;
// const TryStmt = nodes.TryStmt;
// const WhileStmt = nodes.WhileStmt;
// const WithStmt = nodes.WithStmt;
// const EmptyStmt = nodes.EmptyStmt;
const StmtListItem = nodes.StmtListItem;

const Expr = nodes.Expr;
const UnaryExpr = nodes.UnaryExpr;
const NewExpr = nodes.NewExpr;
const SeqExpr = nodes.SeqExpr;
const RegExp = nodes.RegExp;

const ArrowFunc = nodes.ArrowFunc;
const ArrowFuncBody = nodes.ArrowFuncBody;
const AssignmentExpr = nodes.AssignmentExpr;
// const BinaryExpr = nodes.BinaryExpr;
// const MemberExpr = nodes.MemberExpr;
// const GroupExpr = nodes.GroupExpr;
// const ArrayInitExpr = nodes.ArrayInitExpr;
// const ObjectInitExpr = nodes.ObjectInitExpr;
// const FunctionExpr = nodes.FunctionExpr;
// const ComputedMemberExpr = nodes.ComputedMemberExpr;
// const ChainExpr = nodes.ChainExpr;
// const UpdateExpr = nodes.UpdateExpr;
// const MetaProperty = nodes.MetaProperty;
// const AwaitExpr = nodes.AwaitExpr;
// const ConditionExpr = nodes.ConditionExpr;
// const YieldExpr = nodes.YieldExpr;
// const VariableDecl = nodes.VariableDecl;
const AssignmentPattern = nodes.AssignmentPattern;
const ObjectPattern = nodes.ObjectPattern;
const ObjectPatternProperty = nodes.ObjectPatternProperty;
const BindingPattern = nodes.BindingPattern;
const BindingIdentifier = nodes.BindingIdentifier;
const BindingPatternIdentifier = nodes.BindingPatternIdentifier;
const PatternWithDefault = nodes.PatternWithDefault;

const VarDeclKind = nodes.VarDeclKind;
const VarDeclarator = nodes.VarDeclarator;

// const TokenIndex = usize;

// const Declaration = nodes.Declaration;
// const VariableDeclarator = nodes.VariableDeclarator;

// const ArgumentListElement = nodes.ArgumentListElement;
// const Arguments = nodes.Arguments;

const Property = nodes.Property;
const PropertyKey = nodes.PropertyKey;
const PropertyValue = nodes.PropertyValue;
// const PropertyKind = nodes.PropertyKind;
// const Params = nodes.Params;
const SpreadElement = nodes.SpreadElement;
// const ObjectExprProperty = nodes.ObjectExprProperty;
const RestElement = nodes.RestElement;
// const TemplateLiteral = nodes.TemplateLiteral;
// const Pattern = nodes.Pattern;
const ArrayPattern = nodes.ArrayPattern;
const ArrayPatternElement = nodes.ArrayPatternElement;

const ArrayExprElement = nodes.ArrayExprElement;
const ArrayExpr = nodes.ArrayExpr;

// --------------------

pub const Parser = struct {
    const Self = @This();

    start: usize = 0,
    cursor: usize = 0,

    allocator: Allocator,
    // program: Program = undefined,

    code: []const u8 = undefined,
    errors: *ArrayList(Error),
    warnings: *ArrayList(Error),
    tokens: *ArrayList(Token),
    options: ParserOptions,

    scanner_instance: Scanner,

    parser_arena: *ArenaAllocator,
    _a: Allocator,

    pub fn init(allocator: Allocator, code: []const u8, options: ParserOptions) Self {
        var parser_arena = allocator.create(ArenaAllocator) catch unreachable;
        parser_arena.* = ArenaAllocator.init(allocator);

        var tokens = allocator.create(ArrayList(Token)) catch unreachable;
        tokens.* = ArrayList(Token).init(allocator);
        var errors = allocator.create(ArrayList(Error)) catch unreachable;
        errors.* = ArrayList(Error).init(allocator);
        var warnings = allocator.create(ArrayList(Error)) catch unreachable;
        warnings.* = ArrayList(Error).init(allocator);

        var scanner_instance = Scanner.init(
            allocator,
            tokens,
            errors,
            warnings,
        );

        const _a = parser_arena.allocator();

        return Self{
            .allocator = allocator,
            .errors = errors,
            .warnings = warnings,
            .options = options,
            .tokens = tokens,
            .parser_arena = parser_arena,
            .code = code,
            .scanner_instance = scanner_instance,
            ._a = _a,
        };
    }

    pub fn parse(p: *Self) !Program {
        _ = p.scanner_instance.scan(p.code);
        // p.scanner_instance.printTokens();

        p.skipWS();
        const start = p.start;
        const stmt_list_items = try p.parseStmtListItemUpto(&[_]TT{TT.EOF});
        const program = Program{ .loc = try p.getLocation(start), .stmt_list_items = stmt_list_items };
        return program;
    }

    fn parseStmtListItemUpto(p: *Parser, tts: []const TT) ![]const StmtListItem {
        var stmts = ArrayList(StmtListItem).init(p._a);
        while (!matchTokensWithToken(tts, p.current().tok_type)) {
            const stmt = try p.parseStmtListItem();
            if (stmt) |_stmt| {
                try stmts.append(_stmt);
            } else {
                break;
            }
        }
        return stmts.items;
    }

    fn parseStmtsUpto(p: *Parser, tts: []const TT) ![]const Stmt {
        var stmts = ArrayList(Stmt).init(p._a);
        while (!matchTokensWithToken(tts, p.current().tok_type)) {
            if (try p.parseStmt()) |stmt| try stmts.append(stmt) else break;
        }
        return stmts.toOwnedSlice();
    }

    fn parseStmtListItem(p: *Parser) !?StmtListItem {
        var tok: Token = p.current();
        switch (tok.tok_type) {
            TT.ExportToken => {
                // TODO: handle export
                unreachable;
            },
            TT.ImportToken => {
                // TODO: Handle Import
                unreachable;
            },
            TT.ConstToken => {
                // TODO: Handle Const declaration
                unreachable;
            },
            TT.LetToken => {
                // TODO: Handle Let declaration
                unreachable;
            },
            TT.FunctionToken => {
                // TODO: Function
                unreachable;
            },
            TT.ClassToken => {
                // TODO: Handle Class
                unreachable;
            },
            TT.LineTerminatorToken => {
                // TODO: Handle Line
                p.advance();
            },
            else => {
                const stmt = try p.parseStmt();
                if (stmt) |_stmt| return StmtListItem{ .stmt = _stmt };
                return StmtListItem{ .raw = try p.parseRaw(&[_]TT{}) };
            },
        }
        return null;
    }

    fn parseStmt(p: *Parser) !?Stmt {
        return switch (p.current().tok_type) {
            // boolean
            TT.TrueToken,
            TT.FalseToken,
            TT.NullToken,
            // number like tokens
            TT.DecimalToken,
            TT.BinaryToken,
            TT.OctalToken,
            TT.OctalTokenWithoutO,
            TT.HexadecimalToken,
            TT.BigIntToken,

            TT.StringToken,

            TT.TemplateStartToken,
            TT.TemplateToken,

            TT.RegExpToken,
            => Stmt{ .expr_stmt = try p.parseExprStmt() },

            TT.SemicolonToken => Stmt{ .empty_stmt = try p.parseEmptyStmt() },
            TT.OpenBraceToken => Stmt{ .block_stmt = try p.parseBlock() },
            TT.OpenParenToken => Stmt{ .expr_stmt = try p.parseExprStmt() },

            // TT.IdentifierToken => if (p.matchAsyncFunction()) Stmt{ .func_decl = try p.parseFuncDecl() } else Stmt{ .labeled_stmt = p.parseLabeledStmt() },
            // TODO: Uncomment above
            TT.IdentifierToken => try p.parseLabeledStmt(),

            TT.BreakToken => {
                // TODO
                unreachable;
            },
            TT.ContinueToken => {
                // TODO
                unreachable;
            },
            TT.DebuggerToken => {
                // TODO
                unreachable;
            },
            TT.DoToken => {
                // TODO
                unreachable;
            },
            TT.ForToken => {
                // TODO
                unreachable;
            },
            TT.FunctionToken => {
                // TODO
                unreachable;
            },
            TT.IfToken => {
                // TODO
                unreachable;
            },
            TT.ReturnToken => {
                // TODO
                unreachable;
            },
            TT.SwitchToken => {
                // TODO
                unreachable;
            },
            TT.ThrowToken => {
                // TODO
                unreachable;
            },
            TT.TryToken => {
                // TODO
                unreachable;
            },
            TT.VarToken => Stmt{ .var_stmt = try p.parseVarStmt() },
            TT.WhileToken => {
                // TODO
                unreachable;
            },
            TT.WithToken => {
                // TODO
                unreachable;
            },
            else => Stmt{ .expr_stmt = try p.parseExprStmt() },
        };
    }

    fn parseExprStmt(p: *Parser) !*ExprStmt {
        const start = p.cursor;
        const expr_stmt = try p.heapInit(ExprStmt{
            .expr = try p.parseExpr(),
        });
        p.consumeSemicolon();
        expr_stmt.loc = try p.getLocation(start);
        return expr_stmt;
    }

    fn parseEmptyStmt(p: *Parser) !*EmptyStmt {
        const start = p.start;
        p.advance();
        return try p.heapInit(EmptyStmt{ .loc = try p.getLocation(start) });
    }

    fn parseVarStmt(p: *Parser) Err!*VarDecl {
        const start = p.cursor;
        _ = p.eat(TT.VarToken, null);
        p.advanceWS();
        var var_decls = try p.parseVarDeclList(false);
        return try p.heapInit(VarDecl{ .loc = try p.getLocation(start), .decls = var_decls, .kind = .Var });
    }

    fn parseVarDeclList(p: *Parser, in_for: bool) ![]const VarDeclarator {
        var decls = ArrayList(VarDeclarator).init(p._a);
        try decls.append(try p.parseVarDeclaration(in_for));
        var curr_tt = p.current().tok_type;
        p.advanceWS();

        while (curr_tt == TT.CommaToken) : (curr_tt = p.current().tok_type) {
            p.advanceWS();
            try decls.append(try p.parseVarDeclaration(in_for));
        }
        return decls.items;
    }

    fn parseVarDeclaration(p: *Parser, in_for: bool) Err!VarDeclarator {
        const start = p.cursor;

        var params: ArrayList(Token) = ArrayList(Token).init(p._a);
        const id = try p.parsePattern(VarDeclKind.Var, &params);

        if (p.options.strict) {
            switch (id) {
                .binding_identifier => |bi| {
                    if (bi.tok_type != TT.IdentifierToken) {
                        // TODO: Error
                    }
                },
                else => {},
            }
        }

        p.advanceWS();

        var initializer: ?Expr = null;
        if (p.current().tok_type == TT.EqToken) {
            p.advanceWS();
            initializer = try p.parseAssignmentExpression();
        } else if (!in_for) {
            switch (id) {
                .binding_identifier => {},
                else => {
                    _ = p.eat(TT.EqToken, null);
                },
            }
        }

        return VarDeclarator{
            .loc = try p.getLocation(start),
            .id = id,
            .init = initializer,
        };
    }

    fn parsePattern(p: *Parser, kind: VarDeclKind, params: *ArrayList(Token)) Err!BindingPatternIdentifier {
        var curr = p.current();
        if (curr.tok_type == TT.OpenBracketToken) { // '['
            const array_pattern = try p.parseArrayPattern(params, kind);
            return BindingPatternIdentifier{
                .binding_pattern = BindingPattern{ .array_pattern = array_pattern },
            };
        } else if (curr.tok_type == TT.OpenBraceToken) { // '{'
            const object_pattern = try p.parseObjectPattern(params, kind);
            return BindingPatternIdentifier{
                .binding_pattern = BindingPattern{ .object_pattern = object_pattern },
            };
        }
        if (curr.tok_type == TT.LetToken and (kind == .Const or kind == .Let)) {
            // TODO: Tolerate Unexpected Error when let is a pattern
        }
        try params.append(curr);
        return BindingPatternIdentifier{ .binding_identifier = curr };
    }

    fn parseArrayPattern(p: *Parser, params: *ArrayList(Token), kind: VarDeclKind) Err!*ArrayPattern {
        const start = p.cursor;
        _ = p.eat(TT.OpenBracketToken, null);
        var elements = ArrayList(?ArrayPatternElement).init(p._a);

        var curr = p.current();
        while (curr.tok_type != TT.CloseBracketToken) { // ']'
            if (curr.tok_type == TT.CommaToken) {
                p.advanceWS();
                try elements.append(null);
                curr = p.current();
                continue;
            } else if (curr.tok_type == TT.EllipsisToken) { // '...'
                try elements.append(ArrayPatternElement{
                    .rest_element = try p.parseBindingRestElement(params, kind),
                });
            } else {
                const pattern_with_default = try p.parsePatternWithDefault(params, kind);
                try elements.append(switch (pattern_with_default) {
                    .assignment_pattern => |ap| ArrayPatternElement{ .assignment_pattern = ap },
                    .binding_identifier => |bi| ArrayPatternElement{ .binding_identifier = bi },
                    .binding_pattern => |bp| ArrayPatternElement{ .binding_pattern = bp },
                });
            }

            p.skipWS();
            if (p.current().tok_type == TT.CommaToken) p.advanceWS();
            curr = p.current();
        }

        _ = p.eat(TT.CloseBracketToken, null);

        if (elements.items.len > 0) {
            if (elements.items[elements.items.len - 1] == null) {
                _ = elements.pop();
            }
        }

        p.advance();

        return try p.heapInit(ArrayPattern{
            .loc = try p.getLocation(start),
            .elements = elements.items,
        });
    }

    fn parsePatternWithDefault(p: *Parser, params: *ArrayList(Token), kind: VarDeclKind) Err!PatternWithDefault {
        const start = p.cursor;
        const pattern = try p.parsePattern(kind, params);

        if (p.match(TT.EqToken)) {
            p.advance();
            const prev_allow_yield = p.options.allow_yield;
            p.options.allow_yield = true;
            const right = try p.parseAssignmentExpression();
            p.options.allow_yield = prev_allow_yield;
            return PatternWithDefault{ .assignment_pattern = try p.heapInit(AssignmentPattern{
                .loc = try p.getLocation(start),
                .right = right,
                .left = pattern,
            }) };
        }

        return switch (pattern) {
            .binding_identifier => |bi| PatternWithDefault{ .binding_identifier = bi },
            .binding_pattern => |bp| PatternWithDefault{ .binding_pattern = bp },
        };
    }

    fn parseBindingRestElement(p: *Parser, params: *ArrayList(Token), kind: VarDeclKind) Err!*RestElement {
        const start = p.cursor;
        _ = p.eat(TT.EllipsisToken, null);
        const pattern = try p.parsePattern(kind, params);
        return try p.heapInit(RestElement{
            .loc = try p.getLocation(start),
            .arg = pattern,
        });
    }

    fn parseObjectPattern(p: *Parser, params: *ArrayList(Token), kind: VarDeclKind) Err!*ObjectPattern {
        const start = p.cursor;
        var properties = ArrayList(ObjectPatternProperty).init(p._a);
        _ = p.eat(TT.OpenBraceToken, null);

        var curr = p.current();
        while (curr.tok_type != TT.CloseBraceToken) {
            if (curr.tok_type == TT.EllipsisToken) {
                try properties.append(ObjectPatternProperty{ .rest_element = try p.parseRestProperty(params) });
            } else {
                try properties.append(ObjectPatternProperty{ .property = try p.parsePropertyPattern(params, kind) });
            }
            p.skipWS();
            if (p.current().tok_type == TT.CommaToken) p.advanceWS();
            curr = p.current();
        }

        return try p.heapInit(ObjectPattern{
            .loc = try p.getLocation(start),
            .properties = properties.items,
        });
    }

    fn parseRestProperty(p: *Parser, params: *ArrayList(Token)) Err!*RestElement {
        const start = p.cursor;
        _ = p.eat(TT.EllipsisToken, null);
        const arg = try p.parsePattern(.Var, params);
        if (p.match(TT.EqToken)) {
            // TODO: Throw default rest property
        }
        return try p.heapInit(RestElement{ .loc = try p.getLocation(start), .arg = arg });
    }

    fn parsePropertyPattern(p: *Parser, params: *ArrayList(Token), kind: VarDeclKind) Err!*Property {
        const start = p.cursor;
        _ = params;
        _ = kind;
        // TODO: Implement this
        return try p.heapInit(Property{
            .loc = try p.getLocation(start),
        });
    }

    fn parseVarIdentifier(p: *Parser, kind: VarDeclKind) Err!Token {
        const tok = p.current();

        if (tok.tok_type == TT.YieldToken) {
            if (p.options.strict) {
                // TODO; tolerate unexpected Token
            } else if (!p.options.allow_yield) {
                // throw error
            }
        } else if (tok.tok_type != TT.IdentifierToken) {
            // TODO: handle error
            if (p.options.strict or tok.tok_type == TT.LetToken or kind != .Var) {
                // throw error
            }
        } else if ((p.options.is_module) and tok.tok_type == TT.AsyncToken) {
            // TOOD: Handle error
        }
        return tok;
    }

    fn parseBlock(p: *Parser) !*Block {
        const start = p.start;
        p.advance();
        var stmts: ArrayList(Stmt) = ArrayList(Stmt).init(p._a);

        var curr_tt = p.current().tok_type;
        while (curr_tt != TT.CloseBraceToken and curr_tt != TT.EOF) {
            // TODO: Parse block stuffs here
            curr_tt = p.advanceAndNext().tok_type;
        }
        if (curr_tt == TT.CloseBraceToken) p.advance();
        return try p.heapInit(Block{
            .loc = try p.getLocation(start),
            .stmts = stmts.items,
        });
    }

    fn parseLabeledStmt(p: *Parser) !Stmt {
        const start = p.cursor;
        var curr = p.current();

        var expr = try p.parseExpr();

        p.skipWS();
        curr = p.current();
        switch (expr) {
            .identifier => {
                if (curr.tok_type == TT.ColonToken) {
                    // TODO: Handle this
                    unreachable;
                } else {
                    p.consumeSemicolon();
                    var expr_stmt = try p.heapInit(ExprStmt{});
                    expr_stmt.loc = try p.getLocation(start);
                    expr_stmt.expr = expr;
                    return Stmt{ .expr_stmt = expr_stmt };
                }
            },
            else => {
                p.consumeLineEndOrSemicolon();
                var expr_stmt = try p.heapInit(ExprStmt{});
                expr_stmt.loc = try p.getLocation(start);
                expr_stmt.expr = expr;
                return Stmt{ .expr_stmt = expr_stmt };
            },
        }
    }

    fn parseExpr(p: *Parser) Err!Expr {
        const start = p.cursor;
        var curr = p.current();

        var expr: Expr = try p.parseAssignmentExpression();
        curr = p.current();
        p.skipWS();

        var exprs = ArrayList(?Expr).init(p._a);
        if (curr.tok_type == TT.CommaToken) try exprs.append(expr);

        while (curr.tok_type == TT.CommaToken) {
            p.advance();
            p.skipWS();

            try exprs.append(try p.parseAssignmentExpression());

            p.skipWS();
            curr = p.current();
        }

        if (exprs.items.len == 0) {
            exprs.deinit();
            return expr;
        }

        return Expr{ .seq_expr = try p.heapInit(SeqExpr{
            .loc = try p.getLocation(start),
            .exprs = exprs.items,
        }) };
    }

    fn parseAssignmentExpression(p: *Parser) Err!Expr {
        const start = p.cursor;

        // TODO: Lots of stuffs to do
        var left_expr = try p.parseConditionalExpr();
        p.skipWS();

        var curr = p.current();
        if (curr.tok_type == TT.EqToken) {
            p.advance();
            p.skipWS();

            curr = p.current();

            switch (left_expr) {
                .identifier => {
                    // TODO: If variable name is not a keyword or restricted word in strict mode
                    var right_expr = try p.parseConditionalExpr();
                    return Expr{
                        .assignment_expr = try p.heapInit(AssignmentExpr{
                            .loc = try p.getLocation(start),
                            .left_expr = left_expr,
                            .right_expr = right_expr,
                        }),
                    };
                },
                else => {
                    const right_expr = try p.parseAssignmentExpression();
                    return Expr{
                        .assignment_expr = try p.heapInit(AssignmentExpr{
                            .loc = try p.getLocation(start),
                            .left_expr = left_expr,
                            .right_expr = right_expr,
                        }),
                    };
                },
            }
        }

        return left_expr;
    }

    fn parseConditionalExpr(p: *Parser) !Expr {
        const start = p.cursor;
        _ = start;

        var expr = try p.parseLogicalOrNullish();
        if (p.current().tok_type == TT.QuestionToken) {
            // TODO: Handle this part
        }
        return expr;
    }

    fn parseLogicalOrNullish(p: *Parser) !Expr {
        const start = p.start;

        _ = start;

        var expr = try p.parseLogicalAnd();
        p.skipWS();

        var curr = p.current();
        if (curr.tok_type == TT.OrToken or curr.tok_type == TT.NullishToken) {
            p.advance();
            var another_expr = try p.parseLogicalOrNullish();
            // TODO: handle another_expr
            _ = another_expr;
        }

        return expr;
    }

    fn parseLogicalAnd(p: *Parser) !Expr {
        const start = p.start;
        _ = start;

        var expr = try p.parseBitwiseOr();
        p.skipWS();

        var curr = p.current();
        if (curr.tok_type == TT.AndToken) {
            p.advance();
            var another_expr = try p.parseLogicalAnd();
            // TODO
            _ = another_expr;
        }
        return expr;
    }

    fn parseBitwiseOr(p: *Parser) !Expr {
        const start = p.start;
        _ = start;
        var expr = try p.parseBitwiseXor();
        p.skipWS();
        var curr = p.current();
        if (curr.tok_type == TT.BitOrToken) {
            p.advance();
            var another_expr = try p.parseBitwiseOr();
            // TODO
            _ = another_expr;
        }
        return expr;
    }

    fn parseBitwiseXor(p: *Parser) !Expr {
        const start = p.start;
        _ = start;

        var expr = try p.parseBitwiseAnd();
        p.skipWS();
        var curr = p.current();
        if (curr.tok_type == TT.BitXorToken) {
            p.advance();
            var another_expr = try p.parseBitwiseXor();
            // TODO
            _ = another_expr;
        }
        return expr;
    }

    fn parseBitwiseAnd(p: *Parser) !Expr {
        const start = p.start;
        _ = start;

        var expr = try p.parseEqualityExpr();
        p.skipWS();
        var curr = p.current();
        if (curr.tok_type == TT.BitAndToken) {
            p.advance();
            var another_expr = try p.parseBitwiseAnd();
            // TODO
            _ = another_expr;
        }
        return expr;
    }

    fn parseEqualityExpr(p: *Parser) !Expr {
        const start = p.start;
        _ = start;

        var expr = try p.parseComparisonExpr();
        p.skipWS();
        var tt = p.current().tok_type;
        switch (tt) {
            TT.EqEqToken, TT.EqEqEqToken, TT.NotEqToken, TT.NotEqEqToken => {
                p.advance();
                var another_expr = try p.parseEqualityExpr();
                _ = another_expr;
                // TODO
            },
            else => {},
        }
        return expr;
    }

    fn parseComparisonExpr(p: *Parser) !Expr {
        const start = p.cursor;
        _ = start;
        var expr = try p.parseAddSubExpr();
        p.skipWS();
        const tt = p.current().tok_type;
        if (tt == TT.LtLtToken or tt == TT.GtGtToken or tt == TT.GtGtGtToken) {
            p.advance();
            var another_expr = try p.parseComparisonExpr();
            _ = another_expr;
            // TODO
        }
        return expr;
    }

    fn parseAddSubExpr(p: *Parser) !Expr {
        const start = p.cursor;
        _ = start;
        var expr = try p.parseMulDiv();
        p.skipWS();
        const tt = p.current().tok_type;
        if (tt == TT.MulToken or tt == TT.DivToken or tt == TT.ModToken) {
            p.advance();
            var another_expr = try p.parseAddSubExpr();
            _ = another_expr;
            // TODO
        }
        return expr;
    }

    fn parseMulDiv(p: *Parser) !Expr {
        const start = p.cursor;
        _ = start;
        var expr = try p.parseExponentiationExpr();
        p.skipWS();
        const tt = p.current().tok_type;
        if (tt == TT.MulToken or tt == TT.DivToken or tt == TT.ModToken) {
            p.advance();
            var another_expr = try p.parseMulDiv();
            _ = another_expr;
            // TODO
        }
        return expr;
    }

    fn parseExponentiationExpr(p: *Parser) !Expr {
        const start = p.cursor;
        _ = start;
        var expr = try p.parsePrefixExpr();
        p.skipWS();
        const tt = p.current().tok_type;
        if (tt == TT.ExpToken) {
            p.advance();
            var another_expr = try p.parseExponentiationExpr();
            _ = another_expr;
            // TODO
        }
        return expr;
    }

    fn parsePrefixExpr(p: *Parser) !Expr {
        const start = p.cursor;
        const curr = p.current();

        switch (curr.tok_type) {
            TT.IdentifierToken => {
                return try p.parsePostfixExpr();
            },
            TT.AwaitToken,
            TT.DeleteToken,
            TT.VoidToken,
            TT.TypeofToken,
            TT.DecrToken,
            TT.IncrToken,
            TT.SubToken,
            TT.AddToken,
            TT.BitNotToken,
            TT.NotToken,
            => {
                const op = p.currentHeap();
                p.advance();
                return Expr{
                    .unary_expr = try p.heapInit(UnaryExpr{
                        .loc = try p.getLocation(start),
                        .op = op,
                        .expr = try p.parsePostfixExpr(),
                    }),
                };
            },
            else => {},
        }

        return try p.parsePostfixExpr();
    }

    fn parsePostfixExpr(p: *Parser) !Expr {
        const start = p.cursor;
        _ = start;

        var expr = try p.parseNewExpr();
        p.skipWS();
        const tt = p.current().tok_type;
        if (tt == TT.IncrToken or tt == TT.DecrToken) {
            // TODO:
            // expr = PostFix
        }
        return expr;
    }

    fn parseNewExpr(p: *Parser) !Expr {
        const start = p.cursor;

        var tt = p.current().tok_type;
        if (tt == TT.NewToken) {
            p.advance();
            p.skipWS();
            const new_expr = try p.heapInit(NewExpr{
                .loc = try p.getLocation(start),
                .expr = try p.parseNewExpr(),
            });
            return Expr{ .new_expr = new_expr };
        }
        return try p.parseFuncCallMemberExpr();
    }

    fn parseFuncCallMemberExpr(p: *Parser) !Expr {
        const start = p.cursor;
        _ = start;

        var expr = p.parseGroupingExpr();
        p.skipWS();

        var curr = p.current();

        switch (curr.tok_type) {
            TT.DotToken => {
                // handle parseFuncCallMemberExpr
            },
            TT.OptChainToken => {
                // handle parseFuncCallMemberExpr
            },
            TT.OpenBracketToken => {
                // parseExpr
            },
            TT.OpenParenToken => {
                // parseExpr
            },
            else => {},
        }

        return expr;
    }

    fn parseGroupingExpr(p: *Parser) !Expr {
        const start = p.cursor;
        var curr = p.current();

        return switch (curr.tok_type) {
            TT.IdentifierToken => {
                const identifier_expr = Expr{ .identifier = p.currentHeap() };
                p.advance();
                p.skipWS();
                curr = p.current();

                if (curr.tok_type == TT.ArrowToken) {
                    p.advance();
                    p.skipWS();
                    curr = p.current();

                    var arrow_func_body: ArrowFuncBody = undefined;
                    if (curr.tok_type == TT.OpenBraceToken) {
                        arrow_func_body = ArrowFuncBody{ .block = try p.parseBlock() };
                    } else {
                        arrow_func_body = ArrowFuncBody{ .expr = try p.parseExpr() };
                    }
                    return Expr{
                        .arrow_func = try p.heapInit(ArrowFunc{
                            .loc = try p.getLocation(start),
                            .args = identifier_expr,
                            .body = try p.heapInit(arrow_func_body),
                        }),
                    };
                } else {
                    return identifier_expr;
                }
            },
            TT.OpenParenToken => {
                var exprs = try p.parseExpr();
                curr = p.current();

                if (curr.tok_type == TT.CloseParenToken) {
                    p.advance();
                    p.skipWS();
                    curr = p.current();

                    if (curr.tok_type == TT.ArrowToken) {
                        p.advance();
                        p.skipWS();
                        curr = p.current();

                        var arrow_func_body: ArrowFuncBody = undefined;
                        if (curr.tok_type == TT.OpenBraceToken) {
                            arrow_func_body = ArrowFuncBody{ .block = try p.parseBlock() };
                        } else {
                            arrow_func_body = ArrowFuncBody{ .expr = try p.parseExpr() };
                        }
                        return Expr{
                            .arrow_func = try p.heapInit(ArrowFunc{
                                .loc = try p.getLocation(start),
                                .args = exprs,
                                .body = try p.heapInit(arrow_func_body),
                            }),
                        };
                    } else {
                        return exprs;
                    }
                } else {
                    // TODO: handle error
                    unreachable;
                }
            },
            TT.DecimalToken,
            TT.BinaryToken,
            TT.OctalToken,
            TT.OctalTokenWithoutO,
            TT.HexadecimalToken,
            TT.BigIntToken,
            => {
                var expr = Expr{ .literal = p.currentHeap() };
                p.advance();
                return expr;
            },

            TT.OpenBracketToken => { // '['
                var expr = try p.parseArrayInitializer();
                return expr;
            },

            TT.RegExpToken => try p.parseRegExpExpr(),
            TT.StringToken => {
                var expr = Expr{ .literal = p.currentHeap() };
                p.advance();
                return expr;
            },
            else => {
                // TODO
                unreachable;
            },
        };
    }

    fn parseRegExpExpr(p: *Parser) !Expr {
        const start = p.cursor;
        const tok_str = p.getCurrTokStr();

        var flag_start = tok_str.len - 1;
        while (tok_str[flag_start] != '/') : (flag_start -= 1) {
            // std.debug.print("{c}\n", .{tok_str[flag_start]});
            // RegExp
        }

        var flags = tok_str[flag_start + 1 ..];
        var pattern = tok_str[1..flag_start];

        p.advance();

        var reg_exp = try p.heapInit(RegExp{
            .loc = try p.getLocation(start),
            .flags = flags,
            .pattern = pattern,
        });
        return Expr{ .reg_exp = reg_exp };
    }

    fn parseRaw(p: *Self, matchers: []const TT) !Raw {
        const start = p.start;
        var tt = p.current().tok_type;
        tt = p.advanceAndNext().tok_type;

        var raw_arr: ArrayList(*Token) = ArrayList(*Token).init(p._a);

        while (!p.isEnd() or !matchTokensWithToken(matchers, tt)) {
            if (p.previous().tok_type == TT.SemicolonToken) break;

            switch (tt) {
                // All the declarations and statements combined and
                TT.ClassToken,
                TT.FunctionToken,
                TT.VarToken,
                TT.ForToken,
                TT.IfToken,
                TT.WhileToken,
                TT.ReturnToken,
                TT.AsyncToken,
                => {
                    break;
                },
                else => {
                    try raw_arr.append(&p.tokens.items[p.cursor]);
                    tt = p.advanceAndNext().tok_type;
                },
            }
        }
        return Raw{ .loc = try p.getLocation(start), .tokens = raw_arr.items };
    }

    fn parseSpreadElement(p: *Parser) !*SpreadElement {
        const start = p.cursor;
        _ = p.eat(TT.EllipsisToken, null);

        var expr = try p.parseAssignmentExpression();
        return try p.heapInit(SpreadElement{
            .loc = try p.getLocation(start),
            .arg = expr,
        });
    }

    /// Parses `[ <expr> , <expr> ]`
    fn parseArrayInitializer(p: *Parser) !Expr {
        const start = p.cursor;
        p.advance();
        p.skipWS();

        var elements = ArrayList(?ArrayExprElement).init(p._a);

        var curr = p.current();
        while (curr.tok_type != TT.CloseBracketToken) { // ']'
            if (curr.tok_type == TT.CommaToken) {
                p.advance();
                try elements.append(null);
                p.skipWS();
                curr = p.current();
                continue;
            } else if (curr.tok_type == TT.EllipsisToken) { // '...'
                const element = try p.parseSpreadElement();
                p.skipWS();
                curr = p.current();
                const arr_expr_element: ?ArrayExprElement = ArrayExprElement{ .spread_element = element };
                try elements.append(arr_expr_element);
            } else {
                const expr = try p.parseAssignmentExpression();
                try elements.append(ArrayExprElement{ .expr = expr });
            }
            p.skipWS();
            curr = p.current();
            if (curr.tok_type == TT.CommaToken) p.advanceWS();
            curr = p.current();
        }

        if (elements.items.len > 0) {
            if (elements.items[elements.items.len - 1] == null) {
                _ = elements.pop();
            }
        }

        p.advance();

        return Expr{ .arr_expr = try p.heapInit(ArrayExpr{
            .loc = try p.getLocation(start),
            .elements = elements.items,
        }) };
    }

    fn matchAsyncFunction(p: *Parser) bool {
        _ = p;
        return false;
    }

    fn skipWS(p: *Self) void {
        var tt = p.current().tok_type;
        while (tt != TT.EOF and (tt == TT.WhitespaceToken or tt == TT.CommentToken)) {
            p.advance();
            tt = p.current().tok_type;
        }
    }

    fn consumeLineEndOrSemicolon(p: *Parser) void {
        p.skipWS();
        var tt = p.current().tok_type;
        if (tt == TT.SemicolonToken or tt == TT.LineTerminatorToken) {
            p.advance();
        } else {
            // TODO: Error
        }
    }

    fn consumeSemicolon(p: *Parser) void {
        p.skipWS();
        var tt = p.current().tok_type;
        if (tt == TT.SemicolonToken) {
            p.advance();
        } else {
            // TODO: Error
        }
    }

    pub fn deinitInternal(p: *Self) void {
        p.parser_arena.deinit();
        p.allocator.destroy(p.parser_arena);
        p.allocator.destroy(p.errors);
        p.allocator.destroy(p.warnings);
        p.allocator.destroy(p.tokens);
    }

    pub fn deinit(p: *Parser) void {
        p.tokens.deinit();
        p.errors.deinit();
        p.warnings.deinit();
        p.deinitInternal();
        p.scanner_instance.deinit();
    }

    fn advanceWS(p: *Parser) void {
        p.advance();
        p.skipWS();
    }

    /// Advance 1 token ahead
    inline fn advance(p: *Parser) void {
        p.cursor += 1;
    }

    /// Check if its the end of the token list
    fn isEnd(p: *Parser) bool {
        return p.tokens.items[p.cursor].tok_type != TT.EOF;
    }

    /// Consumes the current token, if token doesn't match TT, add Error,
    /// if (matcher != null) Compare the token_string with matcher. Add Error if mismatch
    /// Moves the cursor by 1 step. Returns the current token before the move
    fn eat(p: *Parser, tt: TT, matcher: ?[]const u8) ?*Token {
        const curr = p.current();
        if (curr.tok_type == tt) {
            if (matcher) |_matcher| {
                const cts = p.getCurrTokStr();
                if (!std.mem.eql(u8, cts, _matcher)) {
                    p.addExpectedFoundError(_matcher, cts);
                    return null;
                }
            }
        } else {
            const expected = std.fmt.allocPrint(p._a, "{}", .{tt}) catch unreachable;
            const found = std.fmt.allocPrint(p._a, "{}", .{curr.tok_type}) catch unreachable;
            p.addExpectedFoundError(expected, found);
            return null;
        }
        p.advance();
        return &p.tokens.items[p.cursor - 1];
    }

    /// Eat Token but don't emit any error
    fn eatNoError(p: *Parser, tt: TT, matcher: ?[]const u8) ?usize {
        const curr = p.current();
        if (curr.tok_type == tt) {
            if (matcher) |_matcher| {
                const cts = p.getCurrTokStr();
                if (!std.mem.eql(u8, cts, _matcher)) return null;
            }
        } else {
            return null;
        }
        p.advance();
        return p.cursor - 1;
    }

    fn match(p: *Parser, tt: TokenType) bool {
        return p.current().tok_type == tt;
    }

    /// Adds a New Error
    fn addError(p: *Parser, _type: ParserErrorType) void {
        const curr = p.current();
        p.errors.append(Error{
            .line = curr.loc.start_line,
            .col = curr.loc.start_col,
            .start = curr.loc.start,
            .end = curr.loc.end,
            .error_type = _type,
        }) catch unreachable;
    }

    fn tolerateError(p: *Parser, _error_type: ParserErrorType) void {
        p.addError(_error_type);
    }

    fn getToken(p: *Parser, token_index: usize) Token {
        return p.tokens.items[token_index];
    }

    fn throwUnexpectedToken(p: *Parser) void {
        p.addError(ParserErrorType{ .UnexpectedToken = p.current() });
    }

    /// Adds an Expected Found Error
    fn addExpectedFoundError(p: *Parser, expected: []const u8, found: []const u8) void {
        const curr = p.current();
        p.errors.append(Error{
            .line = curr.loc.start_line,
            .col = curr.loc.start_col,
            .start = curr.loc.start,
            .end = curr.loc.end,
            .error_type = ParserErrorType{
                .ExpectedFound = .{ .expected = expected, .found = found },
            },
        }) catch unreachable;
    }

    /// Advance one step and get the next token
    fn advanceAndNext(p: *Parser) Token {
        p.advance();
        return p.current();
    }

    /// Print Current Token with
    fn printCurrentTok(p: *Parser, extra_info: ?[]const u8) void {
        std.debug.print("{s}\t==> TOKEN_TYPE: {}, string: {s}\n", .{ if (extra_info) |n| n else "", p.current().tok_type, p.getCurrTokStr() });
    }

    /// Get the string value of the token under the cursor
    fn getCurrTokStr(p: *Parser) []const u8 {
        const curr = p.current();
        return p.code[curr.loc.start..curr.loc.end];
    }

    /// Get the current token under the cursor
    fn current(p: *Parser) Token {
        return p.tokens.items[p.cursor];
    }

    fn currentHeap(p: *Parser) *Token {
        return &p.tokens.items[p.cursor];
    }

    /// Look one step ahead and get the
    fn lookAhead(p: *Parser) Token {
        if (p.cursor + 1 >= p.tokens.items.len) return p.tokens.items[p.tokens.items.len - 1];
        return p.tokens.items[p.cursor + 1];
    }

    fn previous(p: *Parser) Token {
        if (p.cursor == 0) return p.tokens.items[0];
        return p.tokens.items[p.cursor - 1];
    }

    /// Get the string of the current
    fn getTokStr(p: *Parser, tok: Token) []const u8 {
        return p.code[tok.start..tok.end];
    }

    /// Allocate the variable `obj` of type `T` on the heap and return
    pub fn heapInit(p: *Parser, obj: anytype) Err!*@TypeOf(obj) {
        const T = @TypeOf(obj);
        const o: *T = try p._a.create(T);
        o.* = obj;
        return o;
    }

    fn matchTokensWithToken(tts: []const TT, tt: TT) bool {
        for (tts) |tt_| if (tt_ == tt) return true;
        return false;
    }

    /// Get the current location object of the node given the `start` of the token index
    ///	in the token list and then update the `p.start` variable to the current cursor
    pub fn getLocation(p: *Parser, start: usize) !*Location {
        const start_tok: Token = p.tokens.items[start];
        const end_tok: Token = p.tokens.items[p.cursor - 1];
        // std.debug.print("\n===\n{d}\n{d}\n===\n", .{start_tok.start, end_tok.end});
        var loc = try p.heapInit(Location{
            .start = start_tok.loc.start,
            .end = end_tok.loc.end,
            .start_line = start_tok.loc.start_line,
            .start_col = start_tok.loc.start_col,
            .end_line = end_tok.loc.end_line,
            .end_col = end_tok.loc.end_col,
        });
        p.start = p.cursor;
        return loc;
    }

    // ----------- The parsing code starts here ----------------
};
