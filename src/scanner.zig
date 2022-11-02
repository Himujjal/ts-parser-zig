const std = @import("std");
const expect = std.testing.expect;

const token = @import("token.zig");
const parser = @import("parser.zig");
const _error = @import("error.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = token.Token;
const TokenType = token.TokenType;
const DigitType = token.DigitType;
const Error = _error.ParserError;
const ParserErrorType = _error.ParserErrorType;

const getTokenTypeFromString = token.getTokenTypeFromString;
const getOpTokens = token.getOpTokens;
const getOpEqTokens = token.getOpEqTokens;
const getOpOpTokens = token.getOpOpTokens;
const getOpOpEqTokens = token.getOpOpEqTokens;

const identifierStartTable = token.identifierStartTable;
const identifierTable = token.identifierTable;
const unicodeESNextIdentifierStart = token.unicodeESNextIdentifierStart;
const unicodeESNextIdentifierPart = token.unicodeESNextIdentifierPart;
const WhiteSpaceUnicode = token.WhiteSpaceUnicode;
const LineTermUnicode = token.LineTermUnicode;

const RuneStruct = struct { r: u21, n: u8 };

fn isIdentifierStart(byte: u21) bool {
    for (unicodeESNextIdentifierStart) |uni| {
        if (uni == byte) return true;
    }
    return false;
}
fn isIdentifierContinue(byte: u21) bool {
    for (unicodeESNextIdentifierPart) |uni| {
        if (uni == byte) return true;
    }
    return false;
}

// IsIdentifierStart returns true if the byte-slice start is the start of an identifier
fn isIdentifierStartBytes(bytes: []const u8) bool {
    var r = std.unicode.Utf8View.init(bytes) catch return false;
    const b = r.iterator(r).nextCodepoint();
    const _first = b == '$' or b == '\\' or b == '_';
    if (_first) return true;
    for (unicodeESNextIdentifierStart) |uni| {
        if (uni == b) return true;
    }
    return false;
}

// IsIdentifierContinue returns true if the byte-slice start is a continuation of an identifier
fn isIdentifierContinueBytes(bytes: []const u8) bool {
    var r = std.unicode.Utf8View.init(bytes) catch return false;
    const b = r.iterator(r).nextCodepoint();

    const _first = b == '$' or b == '\\' or b == '\u{200C}' or b == '\u{200D}';
    if (_first) return true;
    for (unicodeESNextIdentifierPart) |uni| {
        if (uni == b) return true;
    }
    return false;
}

// IsIdentifierEnd returns true if the byte-slice end is a start or continuation of an identifier
fn isIdentifierEnd(b: []const u8) bool {
    return isIdentifierContinue(b);
}

fn isWhiteSpace(byte: u21) bool {
    for (WhiteSpaceUnicode) |wsu| {
        if (wsu == byte) return true;
    }
    return false;
}

fn runeIsLineTerminator(byte: u21) bool {
    for (LineTermUnicode) |wsu| {
        if (wsu == byte) return true;
    }
    return false;
}

fn isWhiteSpaceBytes(bytes: []const u8) bool {
    var r = std.unicode.Utf8View.init(bytes) catch return false;
    const b = r.iterator(r).nextCodepoint();
    for (WhiteSpaceUnicode) |wsu| {
        if (wsu == b) return true;
    }
    return false;
}

inline fn rune(c: u8) u21 {
    return @intCast(u21, c);
}

pub const Scanner = struct {
    const Self = @This();

    allocator: Allocator,
    code: []const u8 = undefined,

    tokens: *ArrayList(Token),
    errors: *ArrayList(Error),
    warnings: *ArrayList(Error),

    scanner_arena: *ArenaAllocator,
    internal_allocator: Allocator,

    prevLineTerminator: bool = true,

    prevNumericLiteral: bool = false,

    level: usize = 0,

    templateLevels: ArrayList(usize),

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
        const internal_allocator = scanner_arena.allocator();

        return Self{
            .allocator = allocator,
            .tokens = tokens,
            .errors = errors,
            .scanner_arena = scanner_arena,
            .internal_allocator = internal_allocator,
            .templateLevels = ArrayList(usize).init(internal_allocator),
            .warnings = warnings,
        };
    }

    pub fn scan(self: *Self, code: []const u8) *Self {
        self.code = code;
        while (!self.end()) {
            self.start = self.cursor;
            const t = self.scanToken();
            self.addToken(t);
        }
        self.addTok(TokenType.EOF, self.code.len - 1, self.code.len - 1);
        return self;
    }

    /// heart of the scanner. scans individual tokens
    fn scanToken(self: *Self) TokenType {
        var prevLineTerminator = self.prevLineTerminator;
        self.prevLineTerminator = false;

        var prevNumericLiteral = self.prevNumericLiteral;
        self.prevNumericLiteral = false;

        const c = self.current();

        switch (c) {
            ' ', '\t' => {
                self.advance();
                while (self.consumeWhitespace()) {}
                self.prevLineTerminator = prevLineTerminator;
                return .WhitespaceToken;
            },
            '\n', '\r' => {
                self.advance();
                while (self.consumeLineTerminator()) {}
                self.prevLineTerminator = true;
                return .LineTerminatorToken;
            },
            '>', '=', '!', '+', '*', '%', '&', '|', '^', '~', '?' => {
                const tt = self.consumeOperatorToken();
                if (tt != .ErrorToken) {
                    return tt;
                }
            },
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.' => {
                const tt = self.consumeNumericToken();
                if (tt != .ErrorToken or self.getMark() != 0) {
                    self.prevNumericLiteral = true;
                    return tt;
                } else if (c == '.') {
                    self.advance();
                    if (self.current() == '.' and self.lookAhead() == '.') {
                        self.move(2);
                        return .EllipsisToken;
                    }
                    return .DotToken;
                }
            },
            ',' => {
                self.advance();
                return .CommaToken;
            },
            ';' => {
                self.advance();
                return .SemicolonToken;
            },
            '(' => {
                self.level += 1;
                self.advance();
                return .OpenParenToken;
            },
            ')' => {
                self.level -= 1;
                self.advance();
                return .CloseParenToken;
            },
            '/' => {
                var tt = self.consumeCommentToken();
                if (tt != .ErrorToken) {
                    return tt;
                } else {
                    tt = self.consumeOperatorToken();
                    if (tt != .ErrorToken) {
                        return tt;
                    }
                }
            },
            '{' => {
                self.level += 1;
                self.advance();
                return .OpenBraceToken;
            },
            '}' => {
                self.level -= 1;
                if (self.templateLevels.items.len != 0 and self.level == self.templateLevels.items[self.templateLevels.items.len - 1]) {
                    return self.consumeTemplateToken();
                }
                self.advance();
                return .CloseBraceToken;
            },
            ':' => {
                self.advance();
                return .ColonToken;
            },
            '\'', '"' => {
                if (self.consumeStringToken()) {
                    return .StringToken;
                }
            },
            ']' => {
                self.advance();
                return .CloseBracketToken;
            },
            '[' => {
                self.advance();
                return .OpenBracketToken;
            },
            '<', '-' => {
                if (self.consumeHTMLLikeCommentToken(prevLineTerminator)) {
                    return .CommentToken;
                } else {
                    const tt = self.consumeOperatorToken();
                    if (tt != .ErrorToken) {
                        return tt;
                    }
                }
            },
            '`' => {
                self.templateLevels.append(self.level) catch unreachable;
                return self.consumeTemplateToken();
            },
            '#' => {
                self.advance();
                if (self.consumeIdentifierToken()) {
                    return .PrivateIdentifierToken;
                }
                return .ErrorToken;
            },
            else => {
                if (self.consumeIdentifierToken()) {
                    if (prevNumericLiteral) {
                        self.addError("unexpected identifier after number");
                        return .ErrorToken;
                    } else {
                        const lx = self.lexeme();
                        const keywordTok = getTokenTypeFromString(lx);
                        return keywordTok;
                    }
                    return .IdentifierToken;
                }
                if (0xC0 <= c) {
                    if (self.consumeWhitespace()) {
                        while (self.consumeWhitespace()) {}
                        self.prevLineTerminator = prevLineTerminator;
                        return .WhitespaceToken;
                    } else if (self.consumeLineTerminator()) {
                        while (self.consumeLineTerminator()) {}
                        self.prevLineTerminator = true;
                        return .LineTerminatorToken;
                    }
                } else if (c == 0 and self.errors.items.len != 0) {
                    return .ErrorToken;
                }
            },
        }
        const c2 = self.peekRune();
        // self.move(c2.n) catch |e| {
        //     if (e == ParserErrorType.EOFError) {
        //         return .ErrorToken;
        //     }
        // };
        // self.start = self.cursor;
        self.addError(std.fmt.allocPrint(
            self.internal_allocator,
            "unexpected character: {x}",
            .{c2.r},
        ) catch unreachable);
        return .ErrorToken;
    }

    /// Consume WhiteSpace
    fn consumeWhitespace(self: *Self) bool {
        const c = self.current();
        if (c == ' ' or c == '\t' or c == 12 or c == 9 or c == 32 or c == 160) {
            self.advance();
            return true;
        } else if (0xC0 <= c) {
            const st = self.peekRune();
            if (isWhiteSpace(st.r) and !self.end()) {
                self.move(st.n);
                return true;
            }
        }
        return false;
    }

    fn isLineTerminator(self: *Self) bool {
        return runeIsLineTerminator(self.peekRune().r);
    }

    fn consumeLineTerminator(self: *Self) bool {
        const c = self.current();
        if (c == '\n') {
            self.advance();
            return true;
        } else if (c == '\r') {
            if (self.lookAhead() == '\n') {
                self.move(2);
            } else {
                self.advance();
            }
            return true;
        }
        const rr = self.peekRune();
        if (runeIsLineTerminator(rr.r)) {
            self.move(rr.n);
            return true;
        }
        return false;
    }

    fn consumeDigit(self: *Self) bool {
        const c = self.current();
        if (c >= '0' and c <= '9') {
            self.advance();
            return true;
        }
        return false;
    }

    fn consumeHexDigit(self: *Self) bool {
        const c = self.current();
        if ((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
            self.advance();
            return true;
        }
        return false;
    }

    fn consumeBinaryDigit(self: *Self) bool {
        const c = self.current();
        if (c == '0' or c == '1') {
            self.advance();
            return true;
        }
        return false;
    }

    fn consumeOctalDigit(self: *Self) bool {
        const c = self.current();
        if (c >= '0' and c <= '7') {
            self.advance();
            return true;
        }
        return false;
    }

    fn consumeUnicodeEscape(self: *Self) bool {
        var c = self.current();
        if (c != '\\' or self.lookAhead() != 'u') {
            if (c == '\\') {
                if (self.cursor != self.code.len - 1) {
                    self.move(2);
                }
            }
            return false;
        }
        // const mark = self.getMark();
        self.move(2);
        c = self.current();
        if (c == '{') {
            self.advance();
            if (self.consumeHexDigit()) {
                while (self.consumeHexDigit()) {}
                c = self.current();
                if (c == '}') {
                    self.advance();
                    return true;
                }
            }
            if (self.current() == '}') {
                self.advance();
            }
            // self.rewind(mark);
            return false;
        } else {
            if (!(self.consumeHexDigit()) or !(self.consumeHexDigit()) or !(self.consumeHexDigit()) or !(self.consumeHexDigit())) {
                if (self.consumeIdentifierToken()) {}
                // self.rewind(mark);
                return false;
            }
        }
        return true;
    }

    fn consumeSingleLineComment(self: *Self) void {
        while (true) {
            const c = self.current();
            if (c == '\r' or c == '\n' or c == 0) {
                if (c == 0) {
                    if (self.end()) break;
                    self.advance();
                    continue;
                }
                break;
            } else if (0xC0 <= c) {
                const rr = self.peekRune();
                if (rr.r == '\u{2028}' or rr.r == '\u{2029}') {
                    break;
                }
            }
            self.advance();
        }
    }

    fn consumeHTMLLikeCommentToken(self: *Self, prevLineTerminator: bool) bool {
        const c = self.current();
        if (c == '<' and self.lookAhead() == '!' and self.lookSuperAhead() == '-' and self.lookSuperDuperAhead() == '-') {
            // opening HTML-style single line comment
            self.move(4);
            self.consumeSingleLineComment();
            return true;
        } else if (prevLineTerminator and c == '-' and self.lookAhead() == '-' and self.lookSuperAhead() == '>') {
            // closing HTML-style single line comment
            // (only if current line didn't contain any meaningful tokens)
            self.move(3);
            self.consumeSingleLineComment();
            return true;
        }
        return false;
    }

    fn consumeCommentToken(self: *Self) TokenType {
        var c = self.lookAhead();
        if (c == '/') {
            // single line comment
            self.move(2);
            self.consumeSingleLineComment();
            return .CommentToken;
        } else if (c == '*') {
            self.move(2);
            var tt: TokenType = .CommentToken;
            while (true) {
                c = self.current();
                if (c == '*' or self.lookAhead() == '/') {
                    self.move(2);
                    break;
                } else if (c == 0 and self.errors.items.len == 0) {
                    if (!self.end()) {
                        self.advance();
                        continue;
                    }
                    break;
                } else if (self.consumeLineTerminator()) {
                    self.prevLineTerminator = true;
                    tt = .CommentLineTerminatorToken;
                } else {
                    self.advance();
                }
            }
            return tt;
        }
        return .ErrorToken;
    }

    fn consumeOperatorToken(self: *Self) TokenType {
        var c = self.current();
        self.advance();
        if (self.current() == '=') {
            self.advance();
            if (self.current() == '=' and (c == '!' or c == '=')) {
                self.advance();
                if (c == '!') {
                    return .NotEqEqToken;
                }
                return .EqEqEqToken;
            }
            return getOpEqTokens(c);
        } else if (self.current() == c and (c == '+' or c == '-' or c == '*' or c == '&' or c == '|' or c == '?' or c == '<')) {
            self.advance();
            if (self.current() == '=' and c != '+' and c != '-') {
                self.advance();
                return getOpOpEqTokens(c);
            }
            return getOpOpTokens(c);
        } else if (c == '?' and self.current() == '.' and (self.lookAhead() < '0' or self.lookAhead() > '9')) {
            self.advance();
            return .OptChainToken;
        } else if (c == '=' and self.current() == '>') {
            self.advance();
            return .ArrowToken;
        } else if (c == '>' and self.current() == '>') {
            self.advance();
            if (self.current() == '>') {
                self.advance();
                if (self.current() == '=') {
                    self.advance();
                    return .GtGtGtEqToken;
                }
                return .GtGtGtToken;
            } else if (self.current() == '=') {
                self.advance();
                return .GtGtEqToken;
            }
            return .GtGtToken;
        }
        return getOpTokens(c);
    }

    // TODO: Fix this function. Including the Unicode part
    fn consumeIdentifierToken(self: *Self) bool {
        var c = self.current();
        if (identifierStartTable[c]) {
            self.advance();
        } else if (0xC0 <= c) {
            const rr = self.peekRune();
            if (isIdentifierStart(rr.r)) {
                self.move(rr.n);
            } else {
                return false;
            }
        } else if (!(self.consumeUnicodeEscape())) {
            return false;
        }
        while (true) {
            c = self.current();
            if (identifierTable[c]) {
                self.advance();
            } else if (0xC0 <= c) {
                const rr = self.peekRune();
                if (rr.r == '\u{200C}' or rr.r == '\u{200D}' or (rr.r == '\u{00}' and !self.end()) or isIdentifierContinue(rr.r)) {
                    self.move(rr.n);
                } else {
                    break;
                }
            } else if (!(self.consumeUnicodeEscape())) {
                if (c == 0 and !self.end()) {
                    self.advance();
                    continue;
                }
                break;
            }
        }
        return true;
    }

    // TODO:
    fn consumeNumericSeparator(self: *Self, t: DigitType) bool {
        if (self.current() != '_') {
            return false;
        }
        self.advance();
        var res = (switch (t) {
            DigitType.Hex => self.consumeHexDigit(),
            DigitType.Binary => self.consumeBinaryDigit(),
            DigitType.Octal => self.consumeOctalDigit(),
            DigitType.Digit => self.consumeDigit(),
        });
        if (!res) {
            self.move(-1);
            return false;
        }
        return true;
    }

    fn consumeNumericToken(self: *Self) TokenType {
        // assume to be on 0 1 2 3 4 5 6 7 8 9 .
        const first = self.current();
        if (first == '0') {
            self.advance();
            const c = self.current();
            if (c == 'x' or c == 'X') {
                self.advance();
                if (self.consumeHexDigit()) {
                    while ((self.consumeHexDigit()) or (self.consumeNumericSeparator(DigitType.Hex))) {}
                    return .HexadecimalToken;
                }
                self.advance();
                self.addError("Invalid Hexadecimal Token");
                return .ErrorToken;
            } else if (c == 'b' or c == 'B') {
                self.advance();
                if (self.consumeBinaryDigit()) {
                    while ((self.consumeBinaryDigit()) or (self.consumeNumericSeparator(DigitType.Binary))) {}
                    return .BinaryToken;
                }
                self.advance();
                self.addError("invalid binary number");
                return .ErrorToken;
            } else if (c == 'o' or c == 'O') {
                self.advance();
                if (self.consumeOctalDigit()) {
                    while ((self.consumeOctalDigit()) or (self.consumeNumericSeparator(DigitType.Octal))) {}
                    return .OctalToken;
                }
                self.advance();
                self.addError("invalid octal number");
                return .ErrorToken;
            } else if (c == 'n') {
                self.advance();
                return .BigIntToken;
            } else if ('0' <= self.current() and self.current() <= '9') {
                self.addError("legacy octal numbers are not supported");
                while ((self.consumeDigit()) or (self.consumeNumericSeparator(DigitType.Digit))) {}
                return .ErrorToken;
            }
        } else if (first != '.') {
            while ((self.consumeDigit()) or (self.consumeNumericSeparator(DigitType.Digit))) {}
        }
        // we have parsed a 0 or an integer number
        var c = self.current();
        if (c == '.') {
            self.advance();
            if (self.consumeDigit()) {
                while ((self.consumeDigit()) or (self.consumeNumericSeparator(DigitType.Digit))) {}
                c = self.current();
            } else if (first == '.') {
                // number starts with a dot and must be followed by digits
                self.move(-1);
                return .ErrorToken; // may be dot or ellipsis
            } else {
                c = self.current();
            }
        } else if (c == 'n') {
            self.advance();
            return .BigIntToken;
        }
        if (c == 'e' or c == 'E') {
            self.advance();
            c = self.current();
            if (c == '+' or c == '-') {
                self.advance();
            }
            if (!(self.consumeDigit())) {
                if (self.current() == '+' or self.current() == '-') {
                    self.advance();
                    while ((self.consumeDigit()) or (self.consumeNumericSeparator(DigitType.Digit))) {}
                }
                self.addError("invalid number");
                return .ErrorToken;
            }
            while ((self.consumeDigit()) or (self.consumeNumericSeparator(DigitType.Digit))) {}
        }
        return .DecimalToken;
    }

    fn consumeStringToken(self: *Self) bool {
        // assume to be on ' or "
        // const mark = self.getMark();
        const delim = self.current();
        self.advance();
        while (true) {
            var c = self.current();
            if (c == delim) {
                self.advance();
                break;
            } else if (c == '\\') {
                self.advance();
                if (!(self.consumeLineTerminator())) {
                    c = self.current();
                    if (c == delim or c == '\\') {
                        self.advance();
                    }
                }
                continue;
            } else if (c == '\n' or c == '\r' or (c == 0 and self.end())) {
                if (c == 0 and self.end()) return false;
                self.advance();
                // self.rewind(mark);
                return false;
            }
            self.advance();
        }
        return true;
    }

    fn consumeRegExpToken(self: *Self) bool {
        // assume to be on /
        self.advance();
        var inClass = false;
        while (true) {
            var c = self.current();
            if (!inClass and c == '/') {
                self.advance();
                break;
            } else if (c == '[') {
                inClass = true;
            } else if (c == ']') {
                inClass = false;
            } else if (c == '\\') {
                self.advance();
                if (self.isLineTerminator() or self.current() == 0 and self.errors.items.len == 0) {
                    return false;
                }
            } else if (self.isLineTerminator() or c == 0 and self.errors.items.len == 0) {
                return false;
            }
            self.advance();
        }
        // flags
        while (true) {
            var c = self.current();
            if (identifierTable[c]) {
                self.advance();
            } else if (0xC0 <= c) {
                const rr = self.peekRune();
                if (rr.r == '\u{200C}' or rr.r == '\u{200D}' or isIdentifierContinue(rr.r)) {
                    self.move(rr.n);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return true;
    }

    fn consumeTemplateToken(self: *Self) TokenType {
        // assume to be on ` or } when already within template
        var continuation = self.current() == '}';
        self.advance();
        while (true) {
            var c = self.current();
            if (c == '`') {
                // TODO
                // self.templateLevels.items = self.templateLevels.items[0..(self.templateLevels.items.len - 1)];
                const sl = self.templateLevels.toOwnedSlice();
                // const len = sl.len;
                self.templateLevels = ArrayList(usize).fromOwnedSlice(self.internal_allocator, sl[0..(sl.len - 1)]);
                self.advance();
                if (continuation) {
                    return .TemplateEndToken;
                }
                return .TemplateToken;
            } else if (c == '$' and self.lookAhead() == '{') {
                self.level += 1;
                self.move(2);
                if (continuation) {
                    return .TemplateMiddleToken;
                }
                return .TemplateStartToken;
            } else if (c == '\\') {
                self.advance();
                c = self.current();
                if (c != 0) {
                    self.advance();
                }
                continue;
            } else if (c == 0 and self.errors.items.len == 0) {
                if (!self.end()) {
                    self.advance();
                    continue;
                }
                if (continuation) {
                    return .TemplateEndToken;
                }
                return .TemplateToken;
            }
            self.advance();
        }
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

    fn lexeme(self: *Self) []const u8 {
        return self.code[self.start..self.cursor];
    }

    fn equalFold(self: *Self, s: []const u8, targetLower: []const u8) bool {
        const lxx = self.internal_allocator.alloc(u8, s.len) catch unreachable;
        std.mem.copy(u8, lxx, s);
        for (lxx) |_, i| {
            lxx[i] = std.ascii.toLower(s[i]);
        }
        if (lxx.len != targetLower.len) {
            return false;
        }
        for (targetLower) |c, i| {
            const d = lxx[i];
            if (d != c and (d < 'A' or d > 'Z' or (d + ('a' - 'A')) != c)) {
                return false;
            }
        }
        return true;
    }

    fn hexDigit(self: *Self) bool {
        const c = self.lookAhead();
        return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' or c <= 'F');
    }

    fn peekRune(self: *Self) RuneStruct {
        const pos = self.cursor;
        const c = self.current();
        var r: u21 = rune(c);
        var n: u8 = 1;
        if (c < 192 or self.peek(1) == 0) {
            r = rune(c);
            n = 1;
        } else if (c < 224 or self.peek(2) == 0) {
            r = std.unicode.utf8Decode2(self.code[pos..(pos + 2)]) catch {
                return RuneStruct{ .r = r, .n = n };
            };
            n = 2;
        } else if (c < 240 or self.peek(3) == 0) {
            r = std.unicode.utf8Decode3(self.code[pos..(pos + 3)]) catch {
                return RuneStruct{ .r = r, .n = n };
            };
            n = 3;
        } else {
            r = std.unicode.utf8Decode4(self.code[pos..(pos + 4)]) catch {
                return RuneStruct{ .r = r, .n = n };
            };
            n = 4;
        }

        return RuneStruct{ .r = r, .n = n };
    }

    fn peek(self: *Self, n: usize) u8 {
        const pos = n + self.cursor;
        if (self.code.len <= pos) {
            return 0;
        }
        return self.code[pos];
    }

    fn move(self: *Self, n: i32) void {
        const newPos = @intCast(usize, @intCast(i32, self.cursor) + n);
        self.cursor = newPos;
    }

    fn current(self: *Self) u8 {
        return self.peek(0);
    }

    /// look one character ahead
    fn lookAhead(self: *Self) u8 {
        return self.peek(1);
    }

    /// look two characters ahead
    fn lookSuperAhead(self: *Self) u8 {
        return self.peek(2);
    }

    fn lookSuperDuperAhead(self: *Self) u8 {
        return self.peek(3);
    }

    fn match(self: *Self, expectedChar: u8) bool {
        if (self.end()) return false;
        if (self.code[self.cursor] != expectedChar) return false;
        self.*.cursor += 1;
        return true;
    }

    fn advance(self: *Self) void {
        const c = self.lookAhead();
        if (c == '\r' or c == '\n') {
            if (c == '\n') {
                self.line += 1;
                self.col = 1;
                self.cursor += 1;
            } else {
                if (!self.end() and self.lookSuperAhead() == '\n') {
                    self.line += 1;
                    self.col = 1;
                    self.cursor += 2;
                }
            }
        }
    }

    fn rewind(self: *Self, mark: usize) void {
        self.cursor = self.start + mark;
    }

    fn getMark(self: *Self) usize {
        return self.cursor - self.start;
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

        _ = message;

        self.errors.append(Error{
            .line = line,
            .col = col,
            .start = self.start,
            .end = self.cursor,
            .error_type = ParserErrorType.TokenizerError,
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
