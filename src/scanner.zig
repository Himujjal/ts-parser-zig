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
const DigitType = token.DigitType;
const Error = _error.Error;
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

fn IsIdentifierStart(byte: u21) bool {
    for (unicodeESNextIdentifierStart) |uni| {
        if (uni == byte) return true;
    }
    return false;
}
fn IsIdentifierContinue(byte: u21) bool {
    for (unicodeESNextIdentifierPart) |uni| {
        if (uni == byte) return true;
    }
    return false;
}

// IsIdentifierStart returns true if the byte-slice start is the start of an identifier
fn IsIdentifierStartBytes(bytes: []const u8) bool {
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
fn IsIdentifierContinueBytes(bytes: []const u8) bool {
    var r = try std.unicode.Utf8View.init(bytes) catch return false;
    const b = r.iterator(r).nextCodepoint();

    const _first = b == '$' or b == '\\' or b == '\u{200C}' or b == '\u{200D}';
    if (_first) return true;
    for (unicodeESNextIdentifierPart) |uni| {
        if (uni == b) return true;
    }
    return false;
}

// IsIdentifierEnd returns true if the byte-slice end is a start or continuation of an identifier
fn IsIdentifierEnd(b: []const u8) bool {
    return IsIdentifierContinue(b);
}

fn IsWhiteSpace(byte: u21) bool {
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

fn IsWhiteSpaceBytes(bytes: []const u8) bool {
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
            const t = self.scanToken() catch unreachable;
            self.addToken(t);
        }
        self.addTok(TokenType.EOF, self.code.len - 1, self.code.len - 1);
        return self;
    }

    /// heart of the scanner. scans individual tokens
    fn scanToken(self: *Self) ParserErrorType!TokenType {
        var prevLineTerminator = self.prevLineTerminator;
        self.prevLineTerminator = false;

        var prevNumericLiteral = self.prevNumericLiteral;
        self.prevNumericLiteral = false;

        const c = self.current();

        switch (c) {
            ' ', '\t' => {
                try self.advance();
                while (try self.consumeWhitespace()) {}
                self.prevLineTerminator = prevLineTerminator;
                return .WhitespaceToken;
            },
            '\n', '\r' => {
                try self.advance();
                while (try self.consumeLineTerminator()) {}
                self.prevLineTerminator = true;
                return .LineTerminatorToken;
            },
            '>', '=', '!', '+', '*', '%', '&', '|', '^', '~', '?' => {
                const tt = try self.consumeOperatorToken();
                if (tt != .ErrorToken) {
                    return tt;
                }
            },
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.' => {
                const tt = try self.consumeNumericToken();
                if (tt != .ErrorToken or self.getMark() != 0) {
                    self.prevNumericLiteral = true;
                    return tt;
                } else if (c == '.') {
                    try self.advance();
                    if (self.current() == '.' and self.lookAhead() == '.') {
                        try self.move(2);
                        return .EllipsisToken;
                    }
                    return .DotToken;
                }
            },
            ',' => {
                try self.advance();
                return .CommaToken;
            },
            ';' => {
                try self.advance();
                return .SemicolonToken;
            },
            '(' => {
                self.level += 1;
                try self.advance();
                return .OpenParenToken;
            },
            ')' => {
                self.level -= 1;
                try self.advance();
                return .CloseParenToken;
            },
            '/' => {
                var tt = try self.consumeCommentToken();
                if (tt != .ErrorToken) {
                    return tt;
                } else {
                    tt = try self.consumeOperatorToken();
                    if (tt != .ErrorToken) {
                        return tt;
                    }
                }
            },
            '{' => {
                self.level += 1;
                try self.advance();
                return .OpenBraceToken;
            },
            '}' => {
                self.level -= 1;
                if (self.templateLevels.items.len != 0 and self.level == self.templateLevels.items[self.templateLevels.items.len - 1]) {
                    return try self.consumeTemplateToken();
                }
                try self.advance();
                return .CloseBraceToken;
            },
            ':' => {
                try self.advance();
                return .ColonToken;
            },
            '\'', '"' => {
                if (try self.consumeStringToken()) {
                    return .StringToken;
                }
            },
            ']' => {
                try self.advance();
                return .CloseBracketToken;
            },
            '[' => {
                try self.advance();
                return .OpenBracketToken;
            },
            '<', '-' => {
                if (try self.consumeHTMLLikeCommentToken(prevLineTerminator)) {
                    return .CommentToken;
                } else {
                    const tt = try self.consumeOperatorToken();
                    if (tt != .ErrorToken) {
                        return tt;
                    }
                }
            },
            '`' => {
                self.templateLevels.append(self.level) catch unreachable;
                return try self.consumeTemplateToken();
            },
            '#' => {
                try self.advance();
                if (try self.consumeIdentifierToken()) {
                    return .PrivateIdentifierToken;
                }
                return .ErrorToken;
            },
            else => {
                if (try self.consumeIdentifierToken()) {
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
                    if (try self.consumeWhitespace()) {
                        while (try self.consumeWhitespace()) {}
                        self.prevLineTerminator = prevLineTerminator;
                        return .WhitespaceToken;
                    } else if (try self.consumeLineTerminator()) {
                        while (try self.consumeLineTerminator()) {}
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
    fn consumeWhitespace(self: *Self) ParserErrorType!bool {
        const c = self.current();
        if (c == ' ' or c == '\t' or c == 12 or c == 9 or c == 32 or c == 160) {
            try self.advance();
            return true;
        } else if (0xC0 <= c) {
            const st = self.peekRune();
            if (IsWhiteSpace(st.r) and !self.end()) {
                try self.move(st.n);
                return true;
            }
        }
        return false;
    }

    fn isLineTerminator(self: *Self) ParserErrorType!bool {
        return runeIsLineTerminator(self.peekRune().r);
    }

    fn consumeLineTerminator(self: *Self) ParserErrorType!bool {
        const c = self.current();
        if (c == '\n') {
            try self.advance();
            return true;
        } else if (c == '\r') {
            if (self.lookAhead() == '\n') {
                try self.move(2);
            } else {
                try self.advance();
            }
            return true;
        }
        const rr = self.peekRune();
        if (runeIsLineTerminator(rr.r)) {
            try self.move(rr.n);
            return true;
        }
        return false;
    }

    fn consumeDigit(self: *Self) ParserErrorType!bool {
        const c = self.current();
        if (c >= '0' and c <= '9') {
            try self.advance();
            return true;
        }
        return false;
    }

    fn consumeHexDigit(self: *Self) ParserErrorType!bool {
        const c = self.current();
        if ((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
            try self.advance();
            return true;
        }
        return false;
    }

    fn consumeBinaryDigit(self: *Self) ParserErrorType!bool {
        const c = self.current();
        if (c == '0' or c == '1') {
            try self.advance();
            return true;
        }
        return false;
    }

    fn consumeOctalDigit(self: *Self) ParserErrorType!bool {
        const c = self.current();
        if (c >= '0' and c <= '7') {
            try self.advance();
            return true;
        }
        return false;
    }

    fn consumeUnicodeEscape(self: *Self) ParserErrorType!bool {
        var c = self.current();
        if (c != '\\' or self.lookAhead() != 'u') {
            if (c == '\\') {
                if (self.cursor != self.code.len - 1) {
                    try self.move(2);
                }
            }
            return false;
        }
        // const mark = self.getMark();
        try self.move(2);
        c = self.current();
        if (c == '{') {
            try self.advance();
            if (try self.consumeHexDigit()) {
                while (try self.consumeHexDigit()) {}
                c = self.current();
                if (c == '}') {
                    try self.advance();
                    return true;
                }
            }
            if (self.current() == '}') {
                try self.advance();
            }
            // self.rewind(mark);
            return false;
        } else {
            if (!(try self.consumeHexDigit()) or !(try self.consumeHexDigit()) or !(try self.consumeHexDigit()) or !(try self.consumeHexDigit())) {
                if (try self.consumeIdentifierToken()) {}
                // self.rewind(mark);
                return false;
            }
        }
        return true;
    }

    fn consumeSingleLineComment(self: *Self) ParserErrorType!void {
        while (true) {
            const c = self.current();
            if (c == '\r' or c == '\n' or c == 0) {
                if (c == 0) {
                    if (self.end()) break;
                    try self.advance();
                    continue;
                }
                break;
            } else if (0xC0 <= c) {
                const rr = self.peekRune();
                if (rr.r == '\u{2028}' or rr.r == '\u{2029}') {
                    break;
                }
            }
            try self.advance();
        }
    }

    fn consumeHTMLLikeCommentToken(self: *Self, prevLineTerminator: bool) ParserErrorType!bool {
        const c = self.current();
        if (c == '<' and self.lookAhead() == '!' and self.lookSuperAhead() == '-' and self.lookSuperDuperAhead() == '-') {
            // opening HTML-style single line comment
            try self.move(4);
            try self.consumeSingleLineComment();
            return true;
        } else if (prevLineTerminator and c == '-' and self.lookAhead() == '-' and self.lookSuperAhead() == '>') {
            // closing HTML-style single line comment
            // (only if current line didn't contain any meaningful tokens)
            try self.move(3);
            try self.consumeSingleLineComment();
            return true;
        }
        return false;
    }

    fn consumeCommentToken(self: *Self) ParserErrorType!TokenType {
        var c = self.lookAhead();
        if (c == '/') {
            // single line comment
            try self.move(2);
            try self.consumeSingleLineComment();
            return .CommentToken;
        } else if (c == '*') {
            try self.move(2);
            var tt: TokenType = .CommentToken;
            while (true) {
                c = self.current();
                if (c == '*' or self.lookAhead() == '/') {
                    try self.move(2);
                    break;
                } else if (c == 0 and self.errors.items.len == 0) {
                    if (!self.end()) {
                        try self.advance();
                        continue;
                    }
                    break;
                } else if (try self.consumeLineTerminator()) {
                    self.prevLineTerminator = true;
                    tt = .CommentLineTerminatorToken;
                } else {
                    try self.advance();
                }
            }
            return tt;
        }
        return .ErrorToken;
    }

    fn consumeOperatorToken(self: *Self) ParserErrorType!TokenType {
        var c = self.current();
        try self.advance();
        if (self.current() == '=') {
            try self.advance();
            if (self.current() == '=' and (c == '!' or c == '=')) {
                try self.advance();
                if (c == '!') {
                    return .NotEqEqToken;
                }
                return .EqEqEqToken;
            }
            return getOpEqTokens(c);
        } else if (self.current() == c and (c == '+' or c == '-' or c == '*' or c == '&' or c == '|' or c == '?' or c == '<')) {
            try self.advance();
            if (self.current() == '=' and c != '+' and c != '-') {
                try self.advance();
                return getOpOpEqTokens(c);
            }
            return getOpOpTokens(c);
        } else if (c == '?' and self.current() == '.' and (self.lookAhead() < '0' or self.lookAhead() > '9')) {
            try self.advance();
            return .OptChainToken;
        } else if (c == '=' and self.current() == '>') {
            try self.advance();
            return .ArrowToken;
        } else if (c == '>' and self.current() == '>') {
            try self.advance();
            if (self.current() == '>') {
                try self.advance();
                if (self.current() == '=') {
                    try self.advance();
                    return .GtGtGtEqToken;
                }
                return .GtGtGtToken;
            } else if (self.current() == '=') {
                try self.advance();
                return .GtGtEqToken;
            }
            return .GtGtToken;
        }
        return getOpTokens(c);
    }

    // TODO: Fix this function. Including the Unicode part
    fn consumeIdentifierToken(self: *Self) ParserErrorType!bool {
        var c = self.current();
        if (identifierStartTable[c]) {
            try self.advance();
        } else if (0xC0 <= c) {
            const rr = self.peekRune();
            if (IsIdentifierStart(rr.r)) {
                try self.move(rr.n);
            } else {
                return false;
            }
        } else if (!(try self.consumeUnicodeEscape())) {
            return false;
        }
        while (true) {
            c = self.current();
            if (identifierTable[c]) {
                try self.advance();
            } else if (0xC0 <= c) {
                const rr = self.peekRune();
                if (rr.r == '\u{200C}' or rr.r == '\u{200D}' or (rr.r == '\u{00}' and !self.end()) or IsIdentifierContinue(rr.r)) {
                    try self.move(rr.n);
                } else {
                    break;
                }
            } else if (!(try self.consumeUnicodeEscape())) {
                if (c == 0 and !self.end()) {
                    try self.advance();
                    continue;
                }
                break;
            }
        }
        return true;
    }

    // TODO:
    fn consumeNumericSeparator(self: *Self, t: DigitType) ParserErrorType!bool {
        if (self.current() != '_') {
            return false;
        }
        try self.advance();
        var res = try (switch (t) {
            DigitType.Hex => self.consumeHexDigit(),
            DigitType.Binary => self.consumeBinaryDigit(),
            DigitType.Octal => self.consumeOctalDigit(),
            DigitType.Digit => self.consumeDigit(),
        });
        if (!res) {
            try self.move(-1);
            return false;
        }
        return true;
    }

    fn consumeNumericToken(self: *Self) ParserErrorType!TokenType {
        // assume to be on 0 1 2 3 4 5 6 7 8 9 .
        const first = self.current();
        if (first == '0') {
            try self.advance();
            const c = self.current();
            if (c == 'x' or c == 'X') {
                try self.advance();
                if (try self.consumeHexDigit()) {
                    while ((try self.consumeHexDigit()) or (try self.consumeNumericSeparator(DigitType.Hex))) {}
                    return .HexadecimalToken;
                }
                try self.advance();
                self.addError("Invalid Hexadecimal Token");
                return .ErrorToken;
            } else if (c == 'b' or c == 'B') {
                try self.advance();
                if (try self.consumeBinaryDigit()) {
                    while ((try self.consumeBinaryDigit()) or (try self.consumeNumericSeparator(DigitType.Binary))) {}
                    return .BinaryToken;
                }
                try self.advance();
                self.addError("invalid binary number");
                return .ErrorToken;
            } else if (c == 'o' or c == 'O') {
                try self.advance();
                if (try self.consumeOctalDigit()) {
                    while ((try self.consumeOctalDigit()) or (try self.consumeNumericSeparator(DigitType.Octal))) {}
                    return .OctalToken;
                }
                try self.advance();
                self.addError("invalid octal number");
                return .ErrorToken;
            } else if (c == 'n') {
                try self.advance();
                return .BigIntToken;
            } else if ('0' <= self.current() and self.current() <= '9') {
                self.addError("legacy octal numbers are not supported");
                while ((try self.consumeDigit()) or (try self.consumeNumericSeparator(DigitType.Digit))) {}
                return .ErrorToken;
            }
        } else if (first != '.') {
            while ((try self.consumeDigit()) or (try self.consumeNumericSeparator(DigitType.Digit))) {}
        }
        // we have parsed a 0 or an integer number
        var c = self.current();
        if (c == '.') {
            try self.advance();
            if (try self.consumeDigit()) {
                while ((try self.consumeDigit()) or (try self.consumeNumericSeparator(DigitType.Digit))) {}
                c = self.current();
            } else if (first == '.') {
                // number starts with a dot and must be followed by digits
                try self.move(-1);
                return .ErrorToken; // may be dot or ellipsis
            } else {
                c = self.current();
            }
        } else if (c == 'n') {
            try self.advance();
            return .BigIntToken;
        }
        if (c == 'e' or c == 'E') {
            try self.advance();
            c = self.current();
            if (c == '+' or c == '-') {
                try self.advance();
            }
            if (!(try self.consumeDigit())) {
                if (self.current() == '+' or self.current() == '-') {
                    try self.advance();
                    while ((try self.consumeDigit()) or (try self.consumeNumericSeparator(DigitType.Digit))) {}
                }
                self.addError("invalid number");
                return .ErrorToken;
            }
            while ((try self.consumeDigit()) or (try self.consumeNumericSeparator(DigitType.Digit))) {}
        }
        return .DecimalToken;
    }

    fn consumeStringToken(self: *Self) ParserErrorType!bool {
        // assume to be on ' or "
        // const mark = self.getMark();
        const delim = self.current();
        try self.advance();
        while (true) {
            var c = self.current();
            if (c == delim) {
                try self.advance();
                break;
            } else if (c == '\\') {
                try self.advance();
                if (!(try self.consumeLineTerminator())) {
                    c = self.current();
                    if (c == delim or c == '\\') {
                        try self.advance();
                    }
                }
                continue;
            } else if (c == '\n' or c == '\r' or (c == 0 and self.end())) {
                if (c == 0 and self.end()) return false;
                try self.advance();
                // self.rewind(mark);
                return false;
            }
            try self.advance();
        }
        return true;
    }

    fn consumeRegExpToken(self: *Self) ParserErrorType!bool {
        // assume to be on /
        try self.advance();
        var inClass = false;
        while (true) {
            var c = self.current();
            if (!inClass and c == '/') {
                try self.advance();
                break;
            } else if (c == '[') {
                inClass = true;
            } else if (c == ']') {
                inClass = false;
            } else if (c == '\\') {
                try self.advance();
                if (self.isLineTerminator() or self.current() == 0 and self.errors.items.len == 0) {
                    return false;
                }
            } else if (self.isLineTerminator() or c == 0 and self.errors.items.len == 0) {
                return false;
            }
            try self.advance();
        }
        // flags
        while (true) {
            var c = self.current();
            if (identifierTable[c]) {
                try self.advance();
            } else if (0xC0 <= c) {
                const rr = self.peekRune();
                if (rr.r == '\u{200C}' or rr.r == '\u{200D}' or IsIdentifierContinue(rr.r)) {
                    try self.move(rr.n);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return true;
    }

    fn consumeTemplateToken(self: *Self) ParserErrorType!TokenType {
        // assume to be on ` or } when already within template
        var continuation = self.current() == '}';
        try self.advance();
        while (true) {
            var c = self.current();
            if (c == '`') {
                // TODO
                // self.templateLevels.items = self.templateLevels.items[0..(self.templateLevels.items.len - 1)];
                const sl = self.templateLevels.toOwnedSlice();
                // const len = sl.len;
                self.templateLevels = ArrayList(usize).fromOwnedSlice(self.internal_allocator, sl[0..(sl.len - 1)]);
                try self.advance();
                if (continuation) {
                    return .TemplateEndToken;
                }
                return .TemplateToken;
            } else if (c == '$' and self.lookAhead() == '{') {
                self.level += 1;
                try self.move(2);
                if (continuation) {
                    return .TemplateMiddleToken;
                }
                return .TemplateStartToken;
            } else if (c == '\\') {
                try self.advance();
                c = self.current();
                if (c != 0) {
                    try self.advance();
                }
                continue;
            } else if (c == 0 and self.errors.items.len == 0) {
                if (!self.end()) {
                    try self.advance();
                    continue;
                }
                if (continuation) {
                    return .TemplateEndToken;
                }
                return .TemplateToken;
            }
            try self.advance();
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

    fn move(self: *Self, n: i32) ParserErrorType!void {
        const newPos = @intCast(usize, @intCast(i32, self.cursor) + n);
        if (self.code.len < newPos) {
            return error.EOFError;
        }
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

    fn advance(self: *Self) ParserErrorType!void {
        try self.move(1);
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

    _ = Scanner.scan(&scanner, code);
    // sc.printTokens();

    return scanner;
}

// --------------------------- TEST -----------------------

const TokenTest = struct {
    ts: []const u8,
    ttypes: []const TokenType,
    lexemes: []const ([]const u8),
};

test "Scanner" {
    const TT = TokenType;

    const WT = TT.WhitespaceToken;

    var tokenTests = [_]TokenTest{
        .{
            // 9 11135 12 160 65279 8192
            .ts = "\t\u{2b7f}\u{000C}\u{00A0}\u{FEFF}\u{2000}",
            .ttypes = &[_]TT{ .WhitespaceToken, .EOF },
            .lexemes = &.{ "\t\u{2b7f}\u{000C}\u{00A0}\u{FEFF}\u{2000}", "" }, // &[_][]const u8{} }, // WhitespaceToken
        },
        .{
            .ts = "\n\r\r\n\u{2028}\u{2029}",
            .ttypes = &[_]TT{ .LineTerminatorToken, .EOF },
            .lexemes = &.{ "\n\r\r\n\u{2028}\u{2029}", "" },
        },
        .{
            .ts = "5.2 .04 1. 2.e3 0x0F 5e99",
            .ttypes = &[_]TT{ .DecimalToken, .WhitespaceToken, .DecimalToken, .WhitespaceToken, .DecimalToken, .WhitespaceToken, .DecimalToken, .WhitespaceToken, .HexadecimalToken, .WhitespaceToken, .DecimalToken, .EOF },
            .lexemes = &.{ "5.2", " ", ".04", " ", "1.", " ", "2.e3", " ", "0x0F", " ", "5e99", "" },
        },
        .{
            .ts = "2_3 5_4.1_2 1_1n 0o2_3 0b1_1 0xF_F",
            .ttypes = &[_]TT{ .DecimalToken, .WhitespaceToken, .DecimalToken, .WhitespaceToken, .BigIntToken, .WhitespaceToken, .OctalToken, .WhitespaceToken, .BinaryToken, .WhitespaceToken, .HexadecimalToken, .EOF },
            .lexemes = &.{ "2_3", " ", "5_4.1_2", " ", "1_1n", " ", "0o2_3", " ", "0b1_1", " ", "0xF_F", "" },
        },
        .{ .ts = "0o22 0b11", .ttypes = &[_]TT{ .OctalToken, .WhitespaceToken, .BinaryToken, .EOF }, .lexemes = &.{ "0o22", " ", "0b11", "" } },
        .{
            .ts = "0n 2345n 435.333n",
            .ttypes = &[_]TT{ .BigIntToken, .WhitespaceToken, .BigIntToken, .WhitespaceToken, .DecimalToken, .ErrorToken, .EOF },
            .lexemes = &.{ "0n", " ", "2345n", " ", "435.333", "n", "" },
        },
        .{
            .ts = "/*comment*/ //comment",
            .ttypes = &[_]TT{ .CommentToken, .WhitespaceToken, .CommentToken, .EOF },
            .lexemes = &.{ "/*comment*/", " ", "//comment", "" },
        },
        .{
            .ts = "{ } ( ) [ ]",
            .ttypes = &[_]TT{ .OpenBraceToken, .WhitespaceToken, .CloseBraceToken, .WhitespaceToken, .OpenParenToken, .WhitespaceToken, .CloseParenToken, .WhitespaceToken, .OpenBracketToken, .WhitespaceToken, .CloseBracketToken, .EOF },
            .lexemes = &.{ "{", " ", "}", " ", "(", " ", ")", " ", "[", " ", "]", "" },
        },
        .{
            .ts = ". ; , < > <= ...",
            .ttypes = &[_]TT{ .DotToken, .WhitespaceToken, .SemicolonToken, .WhitespaceToken, .CommaToken, .WhitespaceToken, .LtToken, .WhitespaceToken, .GtToken, .WhitespaceToken, .LtEqToken, .WhitespaceToken, .EllipsisToken, .EOF },
            .lexemes = &.{ ".", " ", ";", " ", ",", " ", "<", " ", ">", " ", "<=", " ", "...", "" },
        },
        .{
            .ts = ">= == != === !==",
            .ttypes = &[_]TT{ .GtEqToken, .WhitespaceToken, .EqEqToken, .WhitespaceToken, .NotEqToken, .WhitespaceToken, .EqEqEqToken, .WhitespaceToken, .NotEqEqToken, .EOF },
            .lexemes = &.{ ">=", " ", "==", " ", "!=", " ", "===", " ", "!==", "" },
        },
        .{
            .ts = "+ - * / % ** ++ --",
            .ttypes = &[_]TT{ .AddToken, .WhitespaceToken, .SubToken, .WhitespaceToken, .MulToken, .WhitespaceToken, .DivToken, .WhitespaceToken, .ModToken, .WhitespaceToken, .ExpToken, .WhitespaceToken, .IncrToken, .WhitespaceToken, .DecrToken, .EOF },
            .lexemes = &.{ "+", " ", "-", " ", "*", " ", "/", " ", "%", " ", "**", " ", "++", " ", "--", "" },
        },
        .{
            .ts = "<< >> >>> & | ^",
            .ttypes = &[_]TT{ .LtLtToken, .WhitespaceToken, .GtGtToken, .WhitespaceToken, .GtGtGtToken, .WhitespaceToken, .BitAndToken, .WhitespaceToken, .BitOrToken, .WhitespaceToken, .BitXorToken, .EOF },
            .lexemes = &.{ "<<", " ", ">>", " ", ">>>", " ", "&", " ", "|", " ", "^", "" },
        },
        .{
            .ts = "! ~ && || ? : ?? ?.",
            .ttypes = &[_]TT{ .NotToken, .WhitespaceToken, .BitNotToken, .WhitespaceToken, .AndToken, .WhitespaceToken, .OrToken, .WhitespaceToken, .QuestionToken, .WhitespaceToken, .ColonToken, .WhitespaceToken, .NullishToken, .WhitespaceToken, .OptChainToken, .EOF },
            .lexemes = &.{ "!", " ", "~", " ", "&&", " ", "||", " ", "?", " ", ":", " ", "??", " ", "?.", "" },
        },
        .{
            .ts = "= += -= *= **= /= %= <<=",
            .ttypes = &[_]TT{ .EqToken, .WhitespaceToken, .AddEqToken, .WhitespaceToken, .SubEqToken, .WhitespaceToken, .MulEqToken, .WhitespaceToken, .ExpEqToken, .WhitespaceToken, .DivEqToken, .WhitespaceToken, .ModEqToken, .WhitespaceToken, .LtLtEqToken, .EOF },
            .lexemes = &.{ "=", " ", "+=", " ", "-=", " ", "*=", " ", "**=", " ", "/=", " ", "%=", " ", "<<=", "" },
        },
        .{
            .ts = ">>= >>>= &= |= ^= =>",
            .ttypes = &[_]TT{ .GtGtEqToken, .WhitespaceToken, .GtGtGtEqToken, .WhitespaceToken, .BitAndEqToken, .WhitespaceToken, .BitOrEqToken, .WhitespaceToken, .BitXorEqToken, .WhitespaceToken, .ArrowToken, .EOF },
            .lexemes = &.{ ">>=", " ", ">>>=", " ", "&=", " ", "|=", " ", "^=", " ", "=>", "" },
        },
        .{
            .ts = "&&= ||= ??=",
            .ttypes = &[_]TT{ .AndEqToken, .WhitespaceToken, .OrEqToken, .WhitespaceToken, .NullishEqToken, .EOF },
            .lexemes = &.{ "&&=", " ", "||=", " ", "??=", "" },
        },
        .{
            .ts = "?.5",
            .ttypes = &[_]TT{ .QuestionToken, .DecimalToken, .EOF },
            .lexemes = &.{ "?", ".5", "" },
        },
        .{
            .ts = "?.a",
            .ttypes = &[_]TT{ .OptChainToken, .IdentifierToken, .EOF },
            .lexemes = &.{ "?.", "a", "" },
        },
        .{
            .ts = "await break case catch class const continue",
            .ttypes = &[_]TT{ .AwaitToken, WT, .BreakToken, WT, .CaseToken, WT, .CatchToken, WT, .ClassToken, WT, .ConstToken, WT, .ContinueToken, .EOF },
            .lexemes = &.{ "await", " ", "break", " ", "case", " ", "catch", " ", "class", " ", "const", " ", "continue", "" },
        },
        .{
            .ts = "debugger default delete do else enum export extends",
            .ttypes = &[_]TT{ .DebuggerToken, WT, .DefaultToken, WT, .DeleteToken, WT, .DoToken, WT, .ElseToken, WT, .EnumToken, WT, .ExportToken, WT, .ExtendsToken, .EOF },
            .lexemes = &.{ "debugger", " ", "default", " ", "delete", " ", "do", " ", "else", " ", "enum", " ", "export", " ", "extends", "" },
        },
        .{
            .ts = "false true finally for function if import in instanceof",
            .ttypes = &[_]TT{ .FalseToken, WT, .TrueToken, WT, .FinallyToken, WT, .ForToken, WT, .FunctionToken, WT, .IfToken, WT, .ImportToken, WT, .InToken, WT, .InstanceofToken, .EOF },
            .lexemes = &.{ "false", " ", "true", " ", "finally", " ", "for", " ", "function", " ", "if", " ", "import", " ", "in", " ", "instanceof", "" },
        },
        .{
            .ts = "new null return super switch this throw true",
            .ttypes = &[_]TT{ .NewToken, WT, .NullToken, WT, .ReturnToken, WT, .SuperToken, WT, .SwitchToken, WT, .ThisToken, WT, .ThrowToken, WT, .TrueToken, .EOF },
            .lexemes = &.{ "new", " ", "null", " ", "return", " ", "super", " ", "switch", " ", "this", " ", "throw", " ", "true", "" },
        },
        .{
            .ts = "try typeof var void while with yield",
            .ttypes = &[_]TT{ .TryToken, WT, .TypeofToken, WT, .VarToken, WT, .VoidToken, WT, .WhileToken, WT, .WithToken, WT, .YieldToken, .EOF },
            .lexemes = &.{ "try", " ", "typeof", " ", "var", " ", "void", " ", "while", " ", "with", " ", "yield", "" },
        },
        .{
            .ts = "implements interface let package private protected public static",
            .ttypes = &[_]TT{ .ImplementsToken, WT, .InterfaceToken, WT, .LetToken, WT, .PackageToken, WT, .PrivateToken, WT, .ProtectedToken, WT, .PublicToken, WT, .StaticToken, .EOF },
            .lexemes = &.{ "implements", " ", "interface", " ", "let", " ", "package", " ", "private", " ", "protected", " ", "public", " ", "static", "" },
        },
        .{
            .ts = "as async from get meta of set target",
            .ttypes = &[_]TT{ .AsToken, WT, .AsyncToken, WT, .FromToken, WT, .GetToken, WT, .MetaToken, WT, .OfToken, WT, .SetToken, WT, .TargetToken, .EOF },
            .lexemes = &.{ "as", " ", "async", " ", "from", " ", "get", " ", "meta", " ", "of", " ", "set", " ", "target", "" },
        },
        .{
            .ts = "#ident",
            .ttypes = &[_]TT{ .PrivateIdentifierToken, .EOF },
            .lexemes = &.{ "#ident", "" },
        },
        // .{
        //     .ts = "/*co\nm\u{2028m}/*ent*/ //co//mment\u2029//comment",
        //     .ttypes = &[_]TT{ .CommentLineTerminatorToken, WT, .CommentToken, WT, .LineTerminatorToken, WT, .CommentToken, .EOF },
        //     .lexemes = &.{ "/*co\nm\u{2028}m/*ent*/ //co//mment\u{2029}//comment", "" },
        // },
        .{
            .ts = "<!-",
            .ttypes = &[_]TT{ .LtToken, .NotToken, .SubToken, .EOF },
            .lexemes = &.{ "<", "!", "-", "" },
        },
        .{
            .ts = "1<!--2\n",
            .ttypes = &[_]TT{ .DecimalToken, .CommentToken, .LineTerminatorToken, .EOF },
            .lexemes = &.{ "1", "<!--2", "\n", "" },
        },
        .{
            .ts = "x=y-->10\n",
            .ttypes = &[_]TT{ .IdentifierToken, .EqToken, .IdentifierToken, .DecrToken, .GtToken, .DecimalToken, .LineTerminatorToken, .EOF },
            .lexemes = &.{ "x", "=", "y", "--", ">", "10", "\n", "" },
        },
        .{
            .ts = "  /*comment*/ -->nothing\n",
            .ttypes = &[_]TT{ WT, .CommentToken, WT, .DecrToken, .GtToken, .IdentifierToken, .LineTerminatorToken, .EOF },
            .lexemes = &.{ "  ", "/*comment*/", " ", "--", ">", "nothing", "\n", "" },
        },
        .{
            .ts = "1 /*comment\nmultiline*/ -->nothing\n",
            .ttypes = &[_]TT{ .DecimalToken, WT, .CommentLineTerminatorToken, WT, .CommentToken, .LineTerminatorToken, .EOF },
            .lexemes = &.{ "1", " ", "/*comment\nmultiline*/", " ", "-->nothing", "\n", "" },
        },
        // .{
        //     // TODO. This is probably not correct
        //     .ts = "$ _\u{200C} \\u{2000} _\\u{200C} \u{200C}",
        //     .ttypes = &[_]TT{ .IdentifierToken, WT, .IdentifierToken, WT, .IdentifierToken, WT, .IdentifierToken, WT, .EOF },
        //     .lexemes = &.{ "$", " ", "_\u{200C}", " ", "\\u{2000}", " ", "_\\u{200C}", " \u{200C}", "" },
        // },
        .{
            .ts = ">>>=>>>>=",
            .ttypes = &[_]TT{ .GtGtGtEqToken, .GtGtGtToken, .GtEqToken, .EOF },
            .lexemes = &.{ ">>>=", ">>>", ">=", "" },
        },
        .{
            .ts = "1/",
            .ttypes = &[_]TT{ .DecimalToken, .DivToken, .EOF },
            .lexemes = &.{ "1", "/", "" },
        },
        .{
            .ts = "1/=",
            .ttypes = &[_]TT{ .DecimalToken, .DivEqToken, .EOF },
            .lexemes = &.{ "1", "/=", "" },
        },
        .{
            .ts = "'str\\i\\'ng'",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "'str\\i\\'ng'", "" },
        },
        .{
            .ts = "'str\\\\'abc",
            .ttypes = &[_]TT{ .StringToken, .IdentifierToken, .EOF },
            .lexemes = &.{ "'str\\\\'", "abc", "" },
        },
        .{
            .ts = "'str\\\ni\\\\u00A0ng'",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "'str\\\ni\\\\u00A0ng'", "" },
        },
        .{
            .ts = "'str\u{2028}\u{2029}ing'",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "'str\u{2028}\u{2029}ing'", "" },
        },

        .{
            .ts = "0b0101 0o0707 0b17",
            .ttypes = &[_]TT{ .BinaryToken, WT, .OctalToken, WT, .BinaryToken, .DecimalToken, .EOF },
            .lexemes = &.{ "0b0101", " ", "0o0707", " ", "0b1", "7", "" },
        },
        .{
            .ts = "`template`",
            .ttypes = &[_]TT{ .TemplateToken, .EOF },
            .lexemes = &.{ "`template`", "" },
        },
        .{
            .ts = "`a${x+y}b`",
            .ttypes = &[_]TT{ .TemplateStartToken, .IdentifierToken, .AddToken, .IdentifierToken, .TemplateEndToken, .EOF },
            .lexemes = &.{ "`a${", "x", "+", "y", "}b`", "" },
        },
        .{
            .ts = "`tmpl${x}tmpl${x}`",
            .ttypes = &[_]TT{ .TemplateStartToken, .IdentifierToken, .TemplateMiddleToken, .IdentifierToken, .TemplateEndToken, .EOF },
            .lexemes = &.{ "`tmpl${", "x", "}tmpl${", "x", "}`", "" },
        },
        .{
            .ts = "`temp\nlate`",
            .ttypes = &[_]TT{ .TemplateToken, .EOF },
            .lexemes = &.{ "`temp\nlate`", "" },
        },
        .{
            .ts = "`outer${{x: 10}}bar${ raw`nested${2}endnest` }end`",
            .ttypes = &[_]TT{ .TemplateStartToken, .OpenBraceToken, .IdentifierToken, .ColonToken, WT, .DecimalToken, .CloseBraceToken, .TemplateMiddleToken, WT, .IdentifierToken, .TemplateStartToken, .DecimalToken, .TemplateEndToken, WT, .TemplateEndToken, .EOF },
            .lexemes = &.{ "`outer${", "{", "x", ":", " ", "10", "}", "}bar${", " ", "raw", "`nested${", "2", "}endnest`", " ", "}end`", "" },
        },
        .{
            .ts = "`tmpl ${ a ? '' : `tmpl2 ${b ? 'b' : 'c'}` }`",
            .ttypes = &[_]TT{ .TemplateStartToken, WT, .IdentifierToken, WT, .QuestionToken, WT, .StringToken, WT, .ColonToken, WT, .TemplateStartToken, .IdentifierToken, WT, .QuestionToken, WT, .StringToken, WT, .ColonToken, WT, .StringToken, .TemplateEndToken, WT, .TemplateEndToken, .EOF },
            .lexemes = &.{ "`tmpl ${", " ", "a", " ", "?", " ", "''", " ", ":", " ", "`tmpl2 ${", "b", " ", "?", " ", "'b'", " ", ":", " ", "'c'", "}`", " ", "}`", "" },
        },

        // early endings
        .{ .ts = "'string", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "'string", "" } },
        .{ .ts = "'\n", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "'\n", "" } },
        .{ .ts = "'\u{2028}", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "'\u{2028}", "" } },
        .{ .ts = "'str\\\u{0020}ing\\0'", .ttypes = &[_]TT{ .StringToken, .EOF }, .lexemes = &.{ "'str\\\u{0020}ing\\0'", "" } },
        .{
            .ts = "'strin\\00g'",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "'strin\\00g'", "" },
        },
        .{
            .ts = "/*comment",
            .ttypes = &[_]TT{ .CommentToken, .EOF },
            .lexemes = &.{ "/*comment", "" },
        },
        .{
            .ts = "a=/regexp",
            .ttypes = &[_]TT{ .IdentifierToken, .EqToken, .DivToken, .IdentifierToken, .EOF },
            .lexemes = &.{ "a", "=", "/", "regexp", "" },
        },
        .{
            .ts = "\\u002",
            .ttypes = &[_]TT{ .ErrorToken, .EOF },
            .lexemes = &.{ "\\u002", "" },
        },
        .{
            .ts = "`template",
            .ttypes = &[_]TT{ .TemplateToken, .EOF },
            .lexemes = &.{ "`template", "" },
        },
        .{
            .ts = "`template${x}template",
            .ttypes = &[_]TT{ .TemplateStartToken, .IdentifierToken, .TemplateEndToken, .EOF },
            .lexemes = &.{ "`template${", "x", "}template", "" },
        },
        .{
            .ts = "a++=1",
            .ttypes = &[_]TT{ .IdentifierToken, .IncrToken, .EqToken, .DecimalToken, .EOF },
            .lexemes = &.{ "a", "++", "=", "1", "" },
        },
        .{
            .ts = "a++==1",
            .ttypes = &[_]TT{ .IdentifierToken, .IncrToken, .EqEqToken, .DecimalToken, .EOF },
            .lexemes = &.{ "a", "++", "==", "1", "" },
        },
        .{
            .ts = "a++===1",
            .ttypes = &[_]TT{ .IdentifierToken, .IncrToken, .EqEqEqToken, .DecimalToken, .EOF },
            .lexemes = &.{ "a", "++", "===", "1", "" },
        },

        // null characters
        .{
            .ts = "'string\x00'return",
            .ttypes = &[_]TT{ .StringToken, .ReturnToken, .EOF },
            .lexemes = &.{ "'string\x00'", "return", "" },
        },
        .{
            .ts = "//comment\x00comment\nreturn",
            .ttypes = &[_]TT{ .CommentToken, .LineTerminatorToken, .ReturnToken, .EOF },
            .lexemes = &.{ "//comment\x00comment", "\n", "return", "" },
        },
        .{
            .ts = "/*comment\x00*/return",
            .ttypes = &[_]TT{ .CommentToken, .ReturnToken, .EOF },
            .lexemes = &.{ "/*comment\x00*/", "return", "" },
        },
        .{
            .ts = "`template\x00`return",
            .ttypes = &[_]TT{ .TemplateToken, .ReturnToken, .EOF },
            .lexemes = &.{ "`template\x00`", "return", "" },
        },
        .{
            .ts = "`template\\\x00`return",
            .ttypes = &[_]TT{ .TemplateToken, .ReturnToken, .EOF },
            .lexemes = &.{ "`template\\\x00`", "return", "" },
        },

        // numbers
        .{ .ts = "0xg", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "0xg", "" } },
        .{ .ts = "0.f", .ttypes = &[_]TT{ .DecimalToken, .ErrorToken, .EOF }, .lexemes = &.{ "0.", "f", "" } },
        .{ .ts = "0bg", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "0bg", "" } },
        .{ .ts = "0og", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "0og", "" } },
        .{ .ts = "010", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "010", "" } }, // Decimal(0) Decimal(10)
        .{ .ts = "50e+-0", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "50e+-0", "" } },
        .{ .ts = "5.a", .ttypes = &[_]TT{ .DecimalToken, .ErrorToken, .EOF }, .lexemes = &.{ "5.", "a", "" } },
        .{ .ts = "5..a", .ttypes = &[_]TT{ .DecimalToken, .DotToken, .IdentifierToken, .EOF }, .lexemes = &.{ "5.", ".", "a", "" } },

        // coverage
        // TODO: Check the one below
        // .{ .ts = "Ø a〉", .ttypes = &[_]TT{ .IdentifierToken, WT, .IdentifierToken, WT, .EOF }, .lexemes = &.{ "Ø", " ", "a", "〉", "" } },
        .{ .ts = "\u{00A0}\u{FEFF}\u{2000}", .ttypes = &[_]TT{ WT, .EOF }, .lexemes = &.{ "\u{00A0}\u{FEFF}\u{2000}", "" } },
        .{ .ts = "\u{2028}\u{2029}", .ttypes = &[_]TT{ .LineTerminatorToken, .EOF }, .lexemes = &.{ "\u{2028}\u{2029}", "" } },
        .{ .ts = "\\u{0029}ident", .ttypes = &[_]TT{ .IdentifierToken, .EOF }, .lexemes = &.{ "\\u{0029}ident", "" } },
        .{ .ts = "\\u{0029FEF}ident", .ttypes = &[_]TT{ .IdentifierToken, .EOF }, .lexemes = &.{ "\\u{0029FEF}ident", "" } },
        .{ .ts = "\\u{}", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "\\u{}", "" } },
        .{ .ts = "\\ugident", .ttypes = &[_]TT{ .ErrorToken, .EOF }, .lexemes = &.{ "\\ugident", "" } },
        .{ .ts = "'str\ring'", .ttypes = &[_]TT{ .ErrorToken, .IdentifierToken, .ErrorToken, .EOF }, .lexemes = &.{ "'str\r", "ing", "'", "" } },
        .{ .ts = "a=/\\\n", .ttypes = &[_]TT{ .IdentifierToken, .EqToken, .DivToken, .ErrorToken, .EOF }, .lexemes = &.{ "a", "=", "/", "\\\n", "" } },
        .{ .ts = "a=/x\n", .ttypes = &[_]TT{ .IdentifierToken, .EqToken, .DivToken, .IdentifierToken, .LineTerminatorToken, .EOF }, .lexemes = &.{ "a", "=", "/", "x", "\n", "" } },
        .{ .ts = "`\\``", .ttypes = &[_]TT{ .TemplateToken, .EOF }, .lexemes = &.{ "`\\``", "" } },
        .{ .ts = "`\\${ 1 }`", .ttypes = &[_]TT{ .TemplateToken, .EOF }, .lexemes = &.{ "`\\${ 1 }`", "" } },
        .{ .ts = "`\\\r\n`", .ttypes = &[_]TT{ .TemplateToken, .EOF }, .lexemes = &.{ "`\\\r\n`", "" } },

        // go fuzz
        .{ .ts = "`", .ttypes = &[_]TT{ .TemplateToken, .EOF }, .lexemes = &.{ "`", "" } },

        // issues
        .{ .ts = "_\u{00}bare_unicode_escape_identifier", .ttypes = &[_]TT{ .IdentifierToken, .EOF }, .lexemes = &.{ "_\u{00}bare_unicode_escape_identifier", "" } }, // tdewolff/minify#449
    };

    const a = std.testing.allocator;
    var scanner = try a.create(Scanner);
    defer a.destroy(scanner);

    scanner.* = Scanner.init(a, undefined, undefined, undefined);
    defer scanner.deinit();

    const MAX = 84;
    for (tokenTests) |tokenTest, i| {
        if (i == MAX) {
            defer {
                scanner.cursor = 0;
                scanner.start = 0;
                scanner.prevLineTerminator = false;
                scanner.prevLineTerminator = true;
                scanner.prevNumericLiteral = false;
                scanner.level = 0;
                scanner.templateLevels.deinit();
                scanner.raw_text_offset = 0;
            }
            var tokens = std.ArrayList(Token).init(a);
            var errors = ArrayList(Error).init(a);
            var warnings = ArrayList(Error).init(a);
            defer tokens.deinit();
            defer errors.deinit();
            defer warnings.deinit();
            scanner.tokens = &tokens;
            scanner.errors = &errors;
            scanner.warnings = &warnings;

            if (i == MAX) std.debug.print("========= CODE: `{s}` ==============\n", .{tokenTest.ts});

            scanner = Scanner.scan(scanner, tokenTest.ts);
            if (i == MAX) scanner.printTokens();

            var flag = true;
            if (scanner.tokens.items.len == tokenTest.ttypes.len) {
                for (scanner.tokens.items) |tok, j| {
                    if (tok.tok_type != tokenTest.ttypes[j]) {
                        std.debug.print("NOT EQUAL TokenType: {s} == {s}\n", .{ tok.tok_type, tokenTest.ttypes[j] });
                        flag = false;
                    }
                }
            } else {
                std.debug.print("\nERROR: scanner.tokens.len ({d}) != tokenTest.ttypes.len ({d})\n", .{ scanner.tokens.items.len, tokenTest.ttypes.len });
                flag = false;
            }

            if (scanner.tokens.items.len == tokenTest.lexemes.len) {
                for (tokenTest.lexemes) |lexeme, j| {
                    const _code = scanner.tokens.items[j].getCodePartOfToken(tokenTest.ts);
                    if (!std.mem.eql(u8, lexeme, _code)) {
                        std.debug.print("NOT EQUAL: {s} == {s}\n", .{ lexeme, _code });
                        flag = false;
                    }
                }
            } else {
                std.debug.print("\n!==> ERROR: scanner.tokens.len ({d}) != tokenTest.lexemes.len ({d}) <==!\n\n", .{ scanner.tokens.items.len, tokenTest.lexemes.len });
                flag = false;
            }

            try expect(flag == true);
        }
    }
}
