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
};

// ------ Nodes --------
const Program = nodes.Program;
const Stmt = nodes.Stmt;
const EmptyStmt = nodes.EmptyStmt;
const Block = nodes.Block;
const ExprStmt = nodes.ExprStmt;
const Raw = nodes.Raw;
// const DirectivePrologue = nodes.DirectivePrologue;
// const WSs = nodes.WSs;

// const ExportStmt = nodes.ExportStmt;
// const ImportStmt = nodes.ImportStmt;
// const VarDeclStmt = nodes.VarDeclStmt;
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
// const NewExpr = nodes.NewExpr;
// const AssignmentExpr = nodes.AssignmentExpr;
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
// const AssignmentPattern = nodes.AssignmentPattern;
// const ObjectPattern = nodes.ObjectPattern;
// const ObjectPatternProperty = nodes.ObjectPatternProperty;
// const BindingPattern = nodes.BindingPattern;

// const TokenIndex = usize;

// const Declaration = nodes.Declaration;
// const VariableDeclarator = nodes.VariableDeclarator;

// const ArgumentListElement = nodes.ArgumentListElement;
// const Arguments = nodes.Arguments;

// const Property = nodes.Property;
// const PropertyKey = nodes.PropertyKey;
// const PropertyValue = nodes.PropertyValue;
// const PropertyKind = nodes.PropertyKind;
// const Params = nodes.Params;
// const SpreadElement = nodes.SpreadElement;
// const ObjectExprProperty = nodes.ObjectExprProperty;
// const RestElement = nodes.RestElement;
// const TemplateLiteral = nodes.TemplateLiteral;
// const Pattern = nodes.Pattern;
// const ArrayPattern = nodes.ArrayPattern;
// const ArrayPatternElement = nodes.ArrayPatternElement;

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

        _ = scanner_instance.scan(code);

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
        p.skipWS();
        const start = p.start;
        const stmt_list_item = try p.parseStmtListItemUpto(&[_]TT{TT.EOF});
        const program = Program{ .loc = try p.getLocation(start), .stmt_list_item = stmt_list_item };
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
        return stmts.toOwnedSlice();
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
            },
            TT.ImportToken => {
                // TODO: Handle Import
            },
            TT.ConstToken, TT.LetToken, TT.VarToken => {
                // TODO: Handle Var declaration
            },
            TT.FunctionToken => {
                // TODO: Function
            },
            TT.ClassToken => {
                // TODO: Handle Class
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
            TT.SemicolonToken => Stmt{ .empty_stmt = try p.parseEmptyStmt() },
            TT.OpenBraceToken => p.parseBlock(),
            else => null,
        };
    }

    fn parseEmptyStmt(p: *Parser) !*EmptyStmt {
        const start = p.start;
        p.advance();
        return try p.heapInit(EmptyStmt, EmptyStmt{ .loc = try p.getLocation(start) });
    }

    fn parseBlock(p: *Parser) !?Stmt {
        const start = p.start;
        p.advance();
        var stmts: ArrayList(Stmt) = ArrayList(Stmt).init(p._a);

        var curr_tt = p.current().tok_type;
        while (curr_tt != TT.CloseBraceToken and curr_tt != TT.EOF) {
            curr_tt = p.advanceAndNext().tok_type;
        }
        if (curr_tt == TT.CloseBraceToken) p.advance();
        return Stmt{ .block_stmt = try p.heapInit(Block, Block{
            .loc = try p.getLocation(start),
            .stmts = stmts.items,
        }) };
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

    fn skipWS(p: *Self) void {
        var tt = p.current().tok_type;
        while (tt != TT.EOF or tt == TT.WhitespaceToken or tt == TT.CommentToken) {
            tt = p.advanceAndNext().tok_type;
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
            const expected = std.fmt.allocPrint(p._a, "{s}", .{tt}) catch unreachable;
            const found = std.fmt.allocPrint(p._a, "{s}", .{curr.tok_type}) catch unreachable;
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
        return p.lookAhead().tok_type == tt;
    }

    /// Adds a New Error
    fn addError(p: *Parser, _type: ParserErrorType) void {
        const curr = p.current();
        p.errors.append(Error{
            .line = curr.start_line,
            .col = curr.start_col,
            .start = curr.start,
            .end = curr.end,
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
            .line = curr.start_line,
            .col = curr.start_col,
            .start = curr.start,
            .end = curr.end,
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

    /// Get the string value of the token under the cursor
    fn getCurrTokStr(p: *Parser) []const u8 {
        const curr = p.current();
        return p.code[curr.start..curr.end];
    }

    /// Get the current token under the cursor
    fn current(p: *Parser) Token {
        return p.tokens.items[p.cursor];
    }

    /// Look one step ahead and get the
    fn lookAhead(p: *Parser) Token {
        if (p.cursor + 1 >= p.tokens.len) return p.tokens.items[p.tokens.items.len - 1];
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
    pub fn heapInit(p: *Parser, comptime T: type, obj: T) !*T {
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
        var loc = try p.heapInit(Location, Location{
            .start = start_tok.start,
            .end = end_tok.end,
            .start_line = start_tok.start_line,
            .start_col = start_tok.start_col,
            .end_line = end_tok.end_line,
            .end_col = end_tok.end_col,
        });
        p.start = p.cursor;
        return loc;
    }

    // ----------- The parsing code starts here ----------------
};
