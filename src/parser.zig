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

const Scope = struct {
	start_token_index: usize,
	end_token_index: usize,
	isFunctionScope: bool,
};

const State = struct {
    a: Allocator = undefined,
    /// Used to signify the start of a potential arrow function
    potentialArrowAt: i32 = -1,

    /// Used by Flow to handle an edge case involving function type parsing.
    noAnonFunctionType: bool = false,

    /// Used by TypeScript to handle ambiguities when parsing conditional types.
    inDisallowConditionalTypesContext: bool = false,

    /// Token store.
    tokens: ArrayList(Token) = undefined,

    /// Array of all observed scopes, ordered by their ending position.
    scopes: ArrayList(Scope) = undefined,

    /// The current position of the tokenizer in the input.
    pos: usize = 0,

    /// Information about the current token.
    type: TokenType = TT.EOF,
    contextualKeyword: ContextualKeyword = ContextualKeyword.NONE,
    start: usize = 0,
    end: usize = 0,

    isType: bool = false,
    scopeDepth: usize = 0,

    /// If the parser is in an error state, then the token is always tt.eof and all functions can
    /// keep executing but should be written so they don't get into an infinite loop in this situation.
    ///
    /// This approach, combined with the ability to snapshot and restore state, allows us to implement
    /// backtracking without exceptions and without needing to explicitly propagate error states
    /// everywhere.
    err: ?Error = null,

	pub fn init(a: Allocator) State {
		return State{
			.a = a,
			.tokens = ArrayList(Token).init(a),
			.scopes = ArrayList(Scope).init(a),
		};
	}

    pub fn getSnapshot(this: *State) StateSnapshot {
        return StateSnapshot{
            this.potentialArrowAt,
            this.noAnonFunctionType,
            this.inDisallowConditionalTypesContext,
            this.tokens.length,
            this.scopes.length,
            this.pos,
            this.type,
            this.contextualKeyword,
            this.start,
            this.end,
            this.isType,
            this.scopeDepth,
            this.err,
        };
    }

    pub fn restoreFromSnapshot(this: *State, snapshot: StateSnapshot) void {
        this.potentialArrowAt = snapshot.potentialArrowAt;
        this.noAnonFunctionType = snapshot.noAnonFunctionType;
        this.inDisallowConditionalTypesContext = snapshot.inDisallowConditionalTypesContext;
        this.tokens.length = snapshot.tokensLength;
        this.scopes.length = snapshot.scopesLength;
        this.pos = snapshot.pos;
        this.type = snapshot.type;
        this.contextualKeyword = snapshot.contextualKeyword;
        this.start = snapshot.start;
        this.end = snapshot.end;
        this.isType = snapshot.isType;
        this.scopeDepth = snapshot.scopeDepth;
        this.err = snapshot.err;
    }
};

const StateSnapshot = struct {
	potentialArrowAt: usize,
    noAnonFunctionType: bool,
    inDisallowConditionalTypesContext: bool,
    tokensLength: usize,
    scopesLength: usize,
    pos: usize,
    type: TokenType,
    contextualKeyword: ContextualKeyword,
    start: usize,
    end: usize,
    isType: bool,
    scopeDepth: usize,
    err: ?Error = null,
};  

// ------ Nodes --------
// const Program = nodes.Program;
// const Block = nodes.Block;
// const Statement = nodes.Statement;
// const DirectivePrologue = nodes.DirectivePrologue;
// const WSs = nodes.WSs;

// const ExportStmt = nodes.ExportStmt;
// const ImportStmt = nodes.ImportStmt;
// const VarDeclStmt = nodes.VarDeclStmt;
// const FunctionDecl = nodes.FunctionDecl;
// const ClassDecl = nodes.ClassDecl;
// const ExprStmt = nodes.ExprStmt;
// const Expr = nodes.Expr;

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
// const StatementListItem = nodes.StatementListItem;

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

    state: State,
    nextContextId: usize = 1,

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

        const scannerInstance = Scanner.init(
            allocator,
            tokens,
            errors,
            warnings,
        );
        _ = scannerInstance.scan();

		const _a = parser_arena.allocator();

        return Self{
            .allocator = allocator,
            .errors = errors,
            .warnings = warnings,
            .options = options,
            .tokens = tokens,
            .parser_arena = parser_arena,
            .code = code,
			.state = State.init(_a),
            ._a = _a,
        };
    }

    pub fn parse(p: *Self) *Self {
        // p.options = options;
        // _ = p.scannerInstance.scan(code);
        // p.program = p.parseProgram();
        // p.program.source_type = if (options.is_script) .SCRIPT else .MODULE;
        return p;
    }

    pub fn deinitInternal(p: *Self) void {
        p.parser_arena.deinit();
        p.allocator.destroy(p.parser_arena);
        p.allocator.destroy(p.errors);
        p.allocator.destroy(p.warnings);
        p.allocator.destroy(p.tokens);
    }

    pub fn deinit(p: *Self) void {
        p.tokens.deinit();
        p.errors.deinit();
        p.warnings.deinit();
        p.deinitInternal();
        p.scannerInstance.deinit();
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
    fn eat(p: *Parser, tt: TT, matcher: ?[]const u8) ?Token {
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
        return p.tokens.items[p.cursor - 1];
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

    pub fn tolerateError(p: *Parser, _error_type: ParserErrorType) void {
        p.addError(_error_type);
    }

    pub fn getToken(p: *Parser, token_index: usize) Token {
        return p.tokens.items[token_index];
    }

    pub fn throwUnexpectedToken(p: *Parser) void {
        p.addError(ParserErrorType{ .UnexpectedToken = p.current() });
    }

    /// Adds an Expected Found Error
    pub fn addExpectedFoundError(p: *Parser, expected: []const u8, found: []const u8) void {
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
    pub fn advanceAndNext(p: *Parser) Token {
        p.advance();
        return p.current();
    }

    /// Get the string value of the token under the cursor
    pub fn getCurrTokStr(p: *Parser) []const u8 {
        const curr = p.current();
        return p.code[curr.start..curr.end];
    }

    /// Get the current token under the cursor
    pub fn current(p: *Parser) Token {
        return p.tokens.items[p.cursor];
    }

    pub fn lookAhead(p: *Parser) Token {
        return p.tokens.items[p.cursor + 1];
    }

    /// Get the string of the current
    pub fn getTokStr(p: *Parser, tok: Token) []const u8 {
        return p.code[tok.start..tok.end];
    }

    /// Allocate the variable `obj` of type `T` on the heap and return
    pub fn heapInit(p: *Parser, comptime T: type, obj: T) !*T {
        const o: T = try p._a.create(T);
        o.* = obj;
        return o;
    }

    /// Get the current location object of the node given the `start` of the token index
    ///	in the token list and then update the `p.start` variable to the current cursor
    pub fn getLocationAdvance(p: *Parser, start: usize) !*Location {
        const start_tok: Token = p.tokens.items[start];
        const end_tok: Token = p.tokens.items[p.cursor];
        var loc = try p.heapInit(Location, Location{
            .start = start_tok.start,
            .end = end_tok.end,
            .start_line = start_tok.start_line,
            .start_col = start_tok.start_col,
        });
        p.start = p.cursor;
        return loc;
    }

    // ----------- The parsing code starts here ----------------
};
