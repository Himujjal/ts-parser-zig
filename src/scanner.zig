const std = @import("std");
const expect = std.testing.expect;

const token = @import("./token.zig");
const parser = @import("./parser.zig");
const _error = @import("./error.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = token.Token;
const TokenType = token.TokenType;
const Error = _error.Error;
const ParserErrorType = _error.ParserErrorType;

pub const Scanner = struct {
    const Self = @This();

    allocator: Allocator,
    code: []const u8 = undefined,

    tokens: *ArrayList(Token),
    errors: *ArrayList(Error),
    warnings: *ArrayList(Error),

    scanner_arena: *ArenaAllocator,
    internal_allocator: Allocator,

    /// start of the cursor that is fixed on conflicts
    start: usize = 0,

    /// cursor that moves on along the source code
    cursor: usize = 0,

    /// line number of the cursor
    line: usize = 0,

    /// column number
    col: usize = 0,

    raw_text_offset: usize = 0,

    pub fn init(
        allocator: Allocator,
        tokens: *ArrayList(Token),
        errors: *ArrayList(Error),
        warnings: *ArrayList(Error),
    ) Self {
        var scanner_arena = allocator.create(ArenaAllocator) catch unreachable;
        scanner_arena.* = ArenaAllocator.init(allocator);

        return Self{
            .allocator = allocator,
            .tokens = tokens,
            .errors = errors,
            .scanner_arena = scanner_arena,
            .internal_allocator = scanner_arena.allocator(),
            .warnings = warnings,
        };
    }

    pub fn scan(self: *Self, code: []const u8) *Self {
        self.code = code;
        while (!self.end()) {
            self.start = self.cursor;
            self.scanToken();
        }
        self.addTok(TokenType.EOF, self.code.len - 1, self.code.len - 1);
        return self;
    }

    /// heart of the scanner. scans individual tokens
    fn scanToken(self: *Self) void {
        const c = self.advance();
        const TT = TokenType;

        switch (c) {
            ' ', '\n', '\r', '\t' => {},
            '(' => self.addToken(TT.OpenParen),
            ')' => self.addToken(TT.CloseParen),
            '{' => self.addToken(TT.OpenBrace),
            '}' => self.addToken(TT.CloseBrace),
            '[' => self.addToken(TT.OpenBracket),
            ']' => self.addToken(TT.CloseBracket),
            ';' => self.addToken(TT.SemiColon),
            ',' => self.addToken(TT.Comma),
            ':' => self.addToken(TT.Colon),
            '~' => self.addToken(TT.BitNot),
            '@' => self.addToken(TT.At),
            '?' => switch (self.lookAhead()) {
                '?' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.QuestionQuestionAssign, 2),
                        else => self.addTokenAdvance(TT.QuestionQuestion, 1),
                    }
                },
                else => self.addToken(TT.Question),
            },
            '+' => switch (self.lookAhead()) {
                '+' => self.addTokenAdvance(TT.PlusPlus, 1),
                '=' => self.addTokenAdvance(TT.PlusAssign, 1),
                else => self.addToken(TT.Plus),
            },
            '-' => switch (self.lookAhead()) {
                '-' => self.addTokenAdvance(TT.MinusMinus, 1),
                '=' => self.addTokenAdvance(TT.MinusAssign, 1),
                else => self.addToken(TT.Minus),
            },
            '=' => switch (self.lookAhead()) {
                '=' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.IdentityEquals, 2),
                        else => self.addTokenAdvance(TT.Equals, 1),
                    }
                },
                else => self.addToken(TT.Assign),
            },
            '*' => switch (self.lookAhead()) {
                '*' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.ExponentAssign, 2),
                        else => self.addTokenAdvance(TT.Exponent, 1),
                    }
                },
                '=' => self.addTokenAdvance(TT.MultiplyAssign, 1),
                else => self.addToken(TT.Multiply),
            },
            '%' => switch (self.lookAhead()) {
                '=' => self.addTokenAdvance(TT.ModulusAssign, 1),
                else => self.addToken(TT.Modulus),
            },
            '/' => switch (self.lookAhead()) {
                '/' => {
                    while (self.lookAhead() != '\n' and !self.end()) {
                        _ = self.advance();
                    }
                    self.addToken(TT.SingleLineComment);
                },
                '*' => {
                    while (self.lookAhead() != '*' and self.lookSuperAhead() != '/' and !self.end()) {
                        _ = self.advance();
                    }
                    self.addToken(TT.MutliLineComment);
                },
                '=' => self.addTokenAdvance(TT.DivideAssign, 1),
                else => self.addToken(TT.Divide),
            },
            '!' => switch (self.lookAhead()) {
                '=' => self.addTokenAdvance(if (self.match('=')) TT.IdentityNotEquals else TT.NotEquals, 1),
                else => self.addToken(TT.Not),
            },
            '&' => switch (self.lookAhead()) {
                '&' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.AndAssign, 2),
                        else => self.addTokenAdvance(TT.And, 1),
                    }
                },
                '=' => self.addTokenAdvance(TT.BitAndAssign, 1),
                else => self.addToken(TT.BitAnd),
            },
            '|' => switch (self.lookAhead()) {
                '|' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.OrAssign, 2),
                        else => self.addTokenAdvance(TT.Or, 1),
                    }
                },
                '=' => self.addTokenAdvance(TT.BitOrAssign, 1),
                else => self.addToken(TT.BitOr),
            },
            '^' => self.addToken(if (self.match('=')) TT.BitXorAssign else TT.BitXor),
            '.' => switch (self.lookAhead()) {
                '.' => {
                    switch (self.lookSuperAhead()) {
                        '.' => self.addTokenAdvance(TT.Ellipsis, 2),
                        else => self.addError("Invalid character. Did you mean '...' or '.' ?"),
                    }
                },
                else => self.addToken(TT.Dot),
            },
            '<' => switch (self.lookAhead()) {
                '=' => self.addTokenAdvance(TT.LessThanEquals, 1),
                '<' => switch (self.lookSuperAhead()) {
                    '=' => self.addTokenAdvance(TT.LeftShiftArithmeticAssign, 2),
                    else => self.addTokenAdvance(TT.LeftShiftArithmetic, 1),
                },
                else => self.addToken(TT.LessThan),
            },
            '>' => switch (self.lookAhead()) {
                '=' => self.addTokenAdvance(TT.GreaterThanEquals, 1),
                '>' => switch (self.lookSuperAhead()) {
                    '=' => self.addTokenAdvance(TT.LeftShiftArithmetic, 2),
                    '>' => switch (self.lookSuperDuperAhead()) {
                        '=' => self.addTokenAdvance(TT.RightShiftLogicalAssign, 3),
                        else => self.addTokenAdvance(TT.RightShiftLogical, 2),
                    },
                    else => self.addTokenAdvance(TT.LeftShiftArithmeticAssign, 1),
                },
                else => self.addToken(TT.GreaterThan),
            },
            '"', '\'' => self.string(c),
            '`' => self.templateLiteral(),
            else => {
                if (std.ascii.isDigit(c)) {
                    self.number();
                } else if (std.ascii.isAlpha(c) or c == '_') {
                    self.identifier();
                } else {
                    self.addError("Unexpected character");
                }
            },
        }
    }

    /// number lexing
    fn number(self: *Self) void {
        while (std.ascii.isDigit(self.lookAhead()))
            _ = self.advance();

        if (self.lookAhead() == '.' and std.ascii.isDigit(self.lookSuperAhead())) {
            _ = self.advance();
            while (std.ascii.isDigit(self.lookAhead())) {
                _ = self.advance();
            }
        }

        self.addToken(TokenType.Number);
    }

    /// string lexing
    /// TODO: add support for advance escape literals
    fn string(self: *Self, c: u8) void {
        var currentChar: u8 = self.lookAhead();
        while (!self.end()) : ({
            _ = self.advance();
            currentChar = self.lookAhead();
        }) {
            if (currentChar == c) {
                break;
            } else if (currentChar == '\\') {
                _ = self.advance();
            }
        }

        if (self.end()) {
            self.addError("Unterminated string");
            return;
        }
        _ = self.advance();

        // trim the surrounding quotes
        self.addTok(TokenType.String, self.start + 1, self.cursor - 1);
    }

    /// TODO:
    /// process identifier and keywords
    fn identifier(self: *Self) void {
        var ahead = self.lookAhead();
        while (std.ascii.isAlNum(ahead) or ahead == '_') : (ahead = self.lookAhead()) {
            _ = self.advance();
        }
        const text = self.code[self.start..self.cursor];
        var tokType = token.getTokenTypeFromString(text);
        self.addToken(tokType);
    }

    /// template literals
    /// TODO: add support for escape sequences.
    /// TODO: use this again during parsing to get the tokens back
    fn templateLiteral(self: *Self) void {
        var c: u8 = self.lookAhead();
        self.addTok(TokenType.TemplateLiteralStart, self.start, self.start + 1);

        while (!self.end()) : ({
            _ = self.advance();
            c = self.lookAhead();
        }) {
            if (c == '$' and self.lookSuperAhead() == '{') {
                self.addTok(TokenType.TemplateLiteralString, self.start + 1, self.cursor);
                _ = self.advance();
                _ = self.advance();
                self.start = self.cursor;

                _ = self.parseRawText();
                _ = self.processRawText(self.start, self.cursor);
                _ = self.tokens.pop();

                // self.addTok(TokenType.TemplateLiteralRawText, self.start, self.cursor);
                self.start = self.cursor;
            }

            if (c == '`') {
                self.addTok(TokenType.TemplateLiteralString, self.start + 1, self.cursor);
                self.addTok(TokenType.TemplateLiteralEnd, self.cursor, self.cursor + 1);
                break;
            } else if (c == '\\') {
                _ = self.advance();
            }
        }

        if (self.end()) {
            self.addError("Unterminated string");
            return;
        }
        _ = self.advance();
    }

    fn processRawText(self: *Self, start: usize, end_p: usize) void {
        var old_code = self.code;
        var old_cursor = self.cursor;
        var old_start = self.start;
        var old_raw_text_offset = self.raw_text_offset;

        self.raw_text_offset = self.raw_text_offset + self.start;
        self.start = 0;
        self.cursor = 0;
        _ = self.scan(old_code[start..end_p]);
        self.raw_text_offset = old_raw_text_offset;

        self.start = old_start;
        self.cursor = old_cursor;
        self.code = old_code;
    }

    fn addTok(self: *Self, tok_type: TokenType, start: usize, endPos: usize) void {
        self.tokens.append(Token{
            .start = start + self.raw_text_offset,
            .end = endPos + self.raw_text_offset,
            .tok_type = tok_type,
        }) catch unreachable;
    }

    fn parseRawText(self: *Self) void {
        var c = self.lookAhead();
        var curlyBracketDepth: usize = 0;
        var string_type: u8 = 0;

        while (!self.end() and (c != '}' or curlyBracketDepth != 0)) {
            if (c == '{' and string_type == 0) {
                curlyBracketDepth += 1;
            }
            if (string_type != 0 and c == '\\') {
                _ = self.advance();
                _ = self.advance();
                c = self.lookAhead();
            }
            if (string_type == c) {
                string_type = 0;
            } else if (string_type == 0 and (c == '"' or c == '\'' or c == '`')) {
                string_type = c;
            }
            _ = self.advance();
            c = self.lookAhead();

            if (c == '}') {
                if (curlyBracketDepth > 0 and string_type == 0) {
                    curlyBracketDepth -= 1;
                } else if (string_type != 0) {
                    _ = self.advance();
                    c = self.lookAhead();
                }
            }
        }
    }

    /// look one character ahead
    fn lookAhead(self: *Self) u8 {
        return if (self.cursor >= self.code.len) 0 else self.code[self.cursor];
    }

    /// look two characters ahead
    fn lookSuperAhead(self: *Self) u8 {
        if (self.cursor >= self.code.len) return 0;
        if (self.cursor + 1 >= self.code.len) return 0;
        return self.code[self.cursor + 1];
    }

    fn lookSuperDuperAhead(self: *Self) u8 {
        if (self.lookSuperAhead() != 0) {
            if (self.cursor + 2 >= self.code.len) return 0;
            return self.code[self.cursor + 2];
        }
        return 0;
    }

    fn match(self: *Self, expectedChar: u8) bool {
        if (self.end()) return false;
        if (self.code[self.cursor] != expectedChar) return false;
        self.*.cursor += 1;
        return true;
    }

    fn advance(self: *Self) u8 {
        self.cursor += 1;
        self.col += 1;
        if (self.code[self.cursor - 1] == '\n') {
            self.line += 1;
            self.col = 1;
        }
        return self.code[self.cursor - 1];
    }

    fn end(self: *Self) bool {
        return self.cursor >= self.code.len;
    }

    pub fn addTokenAdvance(self: *Self, tok_type: TokenType, steps: usize) void {
        self.cursor += steps;
        self.addToken(tok_type);
    }

    pub fn addToken(self: *Self, tok_type: TokenType) void {
        self.addTok(tok_type, self.start, self.cursor);
    }

    pub fn addError(self: *Self, message: []const u8) void {
        var line: usize = 1;
        var col: usize = 1;
        var i: usize = 0;
        while (i < self.start) : (i += 1) {
            if (self.code[i] == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        const tok = self.code[self.start..self.cursor];

        const errorMessage = std.fmt.allocPrint(
            self.internal_allocator,
            "[{d}:{d}] {s} at `{s}`",
            .{
                line,
                col,
                message,
                tok,
            },
        ) catch unreachable;

        self.errors.append(Error{
            .line = line,
            .startPosition = self.start,
            .endPosition = self.cursor,
            .errorMessage = errorMessage,
            .errorType = ParserErrorType.TokenizerError,
        }) catch unreachable;
    }

    // Only for debugging purposes
    pub fn printTokens(self: *Self) void {
        std.debug.print("========= TOKENS ===========\nToken length: {d}\n", .{self.tokens.items.len});
        for (self.tokens.items) |tok| {
            std.debug.print("{s}\n", .{
                tok.toString(self.internal_allocator, self.code),
            });
        }
        std.debug.print("====================\n", .{});
    }

    pub fn deinitInternal(self: *Self) void {
        self.scanner_arena.deinit();
        self.allocator.destroy(self.scanner_arena);
    }

    pub fn deinit(self: *Self) void {
        self.deinitInternal();
    }
};

fn scannerForTestDeinit(sc: *Scanner) void {
    sc.tokens.deinit();
    sc.errors.deinit();
    sc.warnings.deinit();
    sc.allocator.destroy(sc.tokens);
    sc.allocator.destroy(sc.errors);
    sc.allocator.destroy(sc.warnings);

    sc.deinit();
}

fn getTokens(a: Allocator, code: []const u8) Scanner {
    var tokens = a.create(ArrayList(Token)) catch unreachable;
    tokens.* = ArrayList(Token).init(a);
    var errors = a.create(ArrayList(Error)) catch unreachable;
    errors.* = ArrayList(Error).init(a);
    var warnings = a.create(ArrayList(Error)) catch unreachable;
    warnings.* = ArrayList(Error).init(a);
    var scanner = Scanner.init(a, tokens, errors, warnings);

    var sc = Scanner.scan(&scanner, code);
    sc.printTokens();

    return scanner;
}

test {
    var a = std.testing.allocator;

    var scanner = getTokens(a, "1 + 1 = 2;");
    defer scannerForTestDeinit(&scanner);
    try expect(scanner.tokens.items.len == 7);

    var scanner2 = getTokens(a, "const a = 1;");
    defer scannerForTestDeinit(&scanner2);
    try expect(scanner2.tokens.items.len == 6);
    try expect(scanner2.tokens.items[0].tok_type == TokenType.Const);

    var scanner3 = getTokens(a, "const a = `Hello World ${`hi`} ${\"Hello\\\" World\"} `;");
    defer scannerForTestDeinit(&scanner3);
    try expect(scanner3.tokens.items.len == 14);

    var scanner4 = getTokens(a, "const a = `Hello World ${`${`hi`}`} `;");
    defer scannerForTestDeinit(&scanner4);
    try expect(scanner4.tokens.items.len == 16);
}
