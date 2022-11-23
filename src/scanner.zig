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

const CodeLocation = token.CodeLocation;

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

    start_line: usize = 1,
    start_col: usize = 0,
    /// line number of the cursor
    end_line: usize = 1,
    /// column number
    end_col: usize = 0,

    raw_text_offset: usize = 0,

    pub fn init(
        allocator: Allocator,
        tokens: *ArrayList(Token),
        errors: *ArrayList(Error),
        warnings: *ArrayList(Error),
    ) Scanner {
        var scanner_arena = allocator.create(ArenaAllocator) catch unreachable;
        scanner_arena.* = ArenaAllocator.init(allocator);
        const internal_allocator = scanner_arena.allocator();

        return Scanner{
            .allocator = allocator,
            .tokens = tokens,
            .errors = errors,
            .scanner_arena = scanner_arena,
            .internal_allocator = internal_allocator,
            .templateLevels = ArrayList(usize).init(internal_allocator),
            .warnings = warnings,
        };
    }

    pub fn scan(s: *Scanner, code: []const u8) *Scanner {
        s.code = code;
        while (!s.end()) {
            s.start = s.cursor;
            const t = s.scanToken();
            s.addToken(t);
            s.start_line = s.end_line;
            s.start_col = s.end_col;
        }
        s.addTok(TokenType.EOF, s.code.len - 1, s.code.len - 1);
        return s;
    }

    /// heart of the scanner. scans individual tokens
    fn scanToken(s: *Scanner) TokenType {
        var prevLineTerminator = s.prevLineTerminator;
        s.prevLineTerminator = false;

        var prevNumericLiteral = s.prevNumericLiteral;
        s.prevNumericLiteral = false;

        const c = s.current();

        switch (c) {
            ' ', '\t' => {
                s.advance();
                while (s.consumeWhitespace()) {}
                s.prevLineTerminator = prevLineTerminator;
                return .WhitespaceToken;
            },
            '\n', '\r' => {
                s.advance();
                while (s.consumeLineTerminator()) {}
                s.prevLineTerminator = true;
                return .LineTerminatorToken;
            },
            '>', '=', '!', '+', '*', '%', '&', '|', '^', '~', '?' => {
                const tt = s.consumeOperatorToken();
                if (tt != .ErrorToken) {
                    return tt;
                }
            },
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.' => {
                const tt = s.consumeNumericToken();
                if (tt != .ErrorToken or s.getMark() != 0) {
                    s.prevNumericLiteral = true;
                    return tt;
                } else if (c == '.') {
                    s.advance();
                    if (s.current() == '.' and s.lookAhead() == '.') {
                        s.move(2);
                        return .EllipsisToken;
                    }
                    return .DotToken;
                }
            },
            ',' => {
                s.advance();
                return .CommaToken;
            },
            ';' => {
                s.advance();
                return .SemicolonToken;
            },
            '(' => {
                s.level += 1;
                s.advance();
                return .OpenParenToken;
            },
            ')' => {
                s.level -= 1;
                s.advance();
                return .CloseParenToken;
            },
            '/' => {
                var tt = s.consumeCommentToken();
                if (tt != .ErrorToken) {
                    return tt;
                } else {
                    if (s.consumeRegExpToken()) {
						return .RegExpToken;
					} else {
                        tt = s.consumeOperatorToken();
                        if (tt != .ErrorToken) {
                            return tt;
                        }
                    }
                }
            },
            '{' => {
                s.level += 1;
                s.advance();
                return .OpenBraceToken;
            },
            '}' => {
                s.level -= 1;
                if (s.templateLevels.items.len != 0 and s.level == s.templateLevels.items[s.templateLevels.items.len - 1]) {
                    return s.consumeTemplateToken();
                }
                s.advance();
                return .CloseBraceToken;
            },
            ':' => {
                s.advance();
                return .ColonToken;
            },
            '\'', '"' => {
                if (s.consumeStringToken()) {
                    return .StringToken;
                }
            },
            ']' => {
                s.advance();
                return .CloseBracketToken;
            },
            '[' => {
                s.advance();
                return .OpenBracketToken;
            },
            '<', '-' => {
                if (s.consumeHTMLLikeCommentToken(prevLineTerminator)) {
                    return .CommentToken;
                } else {
                    const tt = s.consumeOperatorToken();
                    if (tt != .ErrorToken) {
                        return tt;
                    }
                }
            },
            '`' => {
                s.templateLevels.append(s.level) catch unreachable;
                return s.consumeTemplateToken();
            },
            '#' => {
                s.advance();
                if (s.consumeIdentifierToken()) {
                    return .PrivateIdentifierToken;
                }
                return .ErrorToken;
            },
            else => {
                if (s.consumeIdentifierToken()) {
                    if (prevNumericLiteral) {
                        s.addError("unexpected identifier after number");
                        return .ErrorToken;
                    } else {
                        const lx = s.lexeme();
                        const keywordTok = getTokenTypeFromString(lx);
                        return keywordTok;
                    }
                    return .IdentifierToken;
                }
                if (0xC0 <= c) {
                    if (s.consumeWhitespace()) {
                        while (s.consumeWhitespace()) {}
                        s.prevLineTerminator = prevLineTerminator;
                        return .WhitespaceToken;
                    } else if (s.consumeLineTerminator()) {
                        while (s.consumeLineTerminator()) {}
                        s.prevLineTerminator = true;
                        return .LineTerminatorToken;
                    }
                } else if (c == 0 and s.errors.items.len != 0) {
                    return .ErrorToken;
                }
            },
        }
        const c2 = s.peekRune();
        // s.move(c2.n) catch |e| {
        //     if (e == ParserErrorType.EOFError) {
        //         return .ErrorToken;
        //     }
        // };
        // s.start = s.cursor;
        s.addError(std.fmt.allocPrint(
            s.internal_allocator,
            "unexpected character: {x}",
            .{c2.r},
        ) catch unreachable);
        return .ErrorToken;
    }

    /// Consume WhiteSpace
    fn consumeWhitespace(s: *Scanner) bool {
        const c = s.current();
        if (c == ' ' or c == '\t' or c == 12 or c == 9 or c == 32 or c == 160) {
            s.advance();
            return true;
        } else if (0xC0 <= c) {
            const st = s.peekRune();
            if (isWhiteSpace(st.r) and !s.end()) {
                s.move(st.n);
                return true;
            }
        }
        return false;
    }

    fn isLineTerminator(s: *Scanner) bool {
        return runeIsLineTerminator(s.peekRune().r);
    }

    fn consumeLineTerminator(s: *Scanner) bool {
        const c = s.current();
        if (c == '\n') {
            s.advance();
            return true;
        } else if (c == '\r') {
            if (s.lookAhead() == '\n') {
                s.move(2);
            } else {
                s.advance();
            }
            return true;
        }
        const rr = s.peekRune();
        if (runeIsLineTerminator(rr.r)) {
            s.move(rr.n);
            return true;
        }
        return false;
    }

    fn consumeDigit(s: *Scanner) bool {
        const c = s.current();
        if (c >= '0' and c <= '9') {
            s.advance();
            return true;
        }
        return false;
    }

    fn consumeHexDigit(s: *Scanner) bool {
        const c = s.current();
        if ((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
            s.advance();
            return true;
        }
        return false;
    }

    fn consumeBinaryDigit(s: *Scanner) bool {
        const c = s.current();
        if (c == '0' or c == '1') {
            s.advance();
            return true;
        }
        return false;
    }

    fn consumeOctalDigit(s: *Scanner) bool {
        const c = s.current();
        if (c >= '0' and c <= '7') {
            s.advance();
            return true;
        }
        return false;
    }

    fn consumeUnicodeEscape(s: *Scanner) bool {
        var c = s.current();
        if (c != '\\' or s.lookAhead() != 'u') {
            if (c == '\\') {
                if (s.cursor != s.code.len - 1) {
                    s.move(2);
                }
            }
            return false;
        }
        // const mark = s.getMark();
        s.move(2);
        c = s.current();
        if (c == '{') {
            s.advance();
            if (s.consumeHexDigit()) {
                while (s.consumeHexDigit()) {}
                c = s.current();
                if (c == '}') {
                    s.advance();
                    return true;
                }
            }
            if (s.current() == '}') {
                s.advance();
            }
            // s.rewind(mark);
            return false;
        } else {
            if (!(s.consumeHexDigit()) or !(s.consumeHexDigit()) or !(s.consumeHexDigit()) or !(s.consumeHexDigit())) {
                if (s.consumeIdentifierToken()) {}
                // s.rewind(mark);
                return false;
            }
        }
        return true;
    }

    fn consumeSingleLineComment(s: *Scanner) void {
        while (true) {
            const c = s.current();
            if (c == '\r' or c == '\n' or c == 0) {
                if (c == 0) {
                    if (s.end()) break;
                    s.advance();
                    continue;
                }
                break;
            } else if (0xC0 <= c) {
                const rr = s.peekRune();
                if (rr.r == '\u{2028}' or rr.r == '\u{2029}') {
                    break;
                }
            }
            s.advance();
        }
    }

    fn consumeHTMLLikeCommentToken(s: *Scanner, prevLineTerminator: bool) bool {
        const c = s.current();
        if (c == '<' and s.lookAhead() == '!' and s.lookSuperAhead() == '-' and s.lookSuperDuperAhead() == '-') {
            // opening HTML-style single line comment
            s.move(4);
            s.consumeSingleLineComment();
            return true;
        } else if (prevLineTerminator and c == '-' and s.lookAhead() == '-' and s.lookSuperAhead() == '>') {
            // closing HTML-style single line comment
            // (only if current line didn't contain any meaningful tokens)
            s.move(3);
            s.consumeSingleLineComment();
            return true;
        }
        return false;
    }

    fn consumeCommentToken(s: *Scanner) TokenType {
        var c = s.lookAhead();
        if (c == '/') {
            // single line comment
            s.move(2);
            s.consumeSingleLineComment();
            return .CommentToken;
        } else if (c == '*') {
            s.move(2);
            var tt: TokenType = .CommentToken;
            while (true) {
                c = s.current();
                if (c == '*' or s.lookAhead() == '/') {
                    s.move(2);
                    break;
                } else if (c == 0 and s.errors.items.len == 0) {
                    if (!s.end()) {
                        s.advance();
                        continue;
                    }
                    break;
                } else if (s.consumeLineTerminator()) {
                    s.prevLineTerminator = true;
                    tt = .CommentLineTerminatorToken;
                } else {
                    s.advance();
                }
            }
            return tt;
        }
        return .ErrorToken;
    }

    fn consumeOperatorToken(s: *Scanner) TokenType {
        var c = s.current();
        s.advance();
        if (s.current() == '=') {
            s.advance();
            if (s.current() == '=' and (c == '!' or c == '=')) {
                s.advance();
                if (c == '!') {
                    return .NotEqEqToken;
                }
                return .EqEqEqToken;
            }
            return getOpEqTokens(c);
        } else if (s.current() == c and (c == '+' or c == '-' or c == '*' or c == '&' or c == '|' or c == '?' or c == '<')) {
            s.advance();
            if (s.current() == '=' and c != '+' and c != '-') {
                s.advance();
                return getOpOpEqTokens(c);
            }
            return getOpOpTokens(c);
        } else if (c == '?' and s.current() == '.' and (s.lookAhead() < '0' or s.lookAhead() > '9')) {
            s.advance();
            return .OptChainToken;
        } else if (c == '=' and s.current() == '>') {
            s.advance();
            return .ArrowToken;
        } else if (c == '>' and s.current() == '>') {
            s.advance();
            if (s.current() == '>') {
                s.advance();
                if (s.current() == '=') {
                    s.advance();
                    return .GtGtGtEqToken;
                }
                return .GtGtGtToken;
            } else if (s.current() == '=') {
                s.advance();
                return .GtGtEqToken;
            }
            return .GtGtToken;
        }
        return getOpTokens(c);
    }

    // TODO: Fix this function. Including the Unicode part
    fn consumeIdentifierToken(s: *Scanner) bool {
        var c = s.current();
        if (identifierStartTable[c]) {
            s.advance();
        } else if (0xC0 <= c) {
            const rr = s.peekRune();
            if (isIdentifierStart(rr.r)) {
                s.move(rr.n);
            } else {
                return false;
            }
        } else if (!(s.consumeUnicodeEscape())) {
            return false;
        }
        while (true) {
            c = s.current();
            if (identifierTable[c]) {
                s.advance();
            } else if (0xC0 <= c) {
                const rr = s.peekRune();
                if (rr.r == '\u{200C}' or rr.r == '\u{200D}' or (rr.r == '\u{00}' and !s.end()) or isIdentifierContinue(rr.r)) {
                    s.move(rr.n);
                } else {
                    break;
                }
            } else if (!(s.consumeUnicodeEscape())) {
                if (c == 0 and !s.end()) {
                    s.advance();
                    continue;
                }
                break;
            }
        }
        return true;
    }

    // TODO:
    fn consumeNumericSeparator(s: *Scanner, t: DigitType) bool {
        if (s.current() != '_') {
            return false;
        }
        s.advance();
        var res = (switch (t) {
            DigitType.Hex => s.consumeHexDigit(),
            DigitType.Binary => s.consumeBinaryDigit(),
            DigitType.Octal => s.consumeOctalDigit(),
            DigitType.Digit => s.consumeDigit(),
        });
        if (!res) {
            s.move(-1);
            return false;
        }
        return true;
    }

    fn consumeNumericToken(s: *Scanner) TokenType {
        // assume to be on 0 1 2 3 4 5 6 7 8 9 .
        const first = s.current();
        if (first == '0') {
            s.advance();
            const c = s.current();
            if (c == 'x' or c == 'X') {
                s.advance();
                if (s.consumeHexDigit()) {
                    while ((s.consumeHexDigit()) or (s.consumeNumericSeparator(DigitType.Hex))) {}
                    return .HexadecimalToken;
                }
                s.advance();
                s.addError("Invalid Hexadecimal Token");
                return .ErrorToken;
            } else if (c == 'b' or c == 'B') {
                s.advance();
                if (s.consumeBinaryDigit()) {
                    while ((s.consumeBinaryDigit()) or (s.consumeNumericSeparator(DigitType.Binary))) {}
                    return .BinaryToken;
                }
                s.advance();
                s.addError("invalid binary number");
                return .ErrorToken;
            } else if (c == 'o' or c == 'O') {
                s.advance();
                if (s.consumeOctalDigit()) {
                    while ((s.consumeOctalDigit()) or (s.consumeNumericSeparator(DigitType.Octal))) {}
                    return .OctalToken;
                }
                s.advance();
                s.addError("invalid octal number");
                return .ErrorToken;
            } else if (c == 'n') {
                s.advance();
                return .BigIntToken;
            } else if ('0' <= s.current() and s.current() <= '9') {
                // s.addError("legacy octal numbers are not supported");
                while ((s.consumeDigit()) or (s.consumeNumericSeparator(DigitType.Digit))) {}
                if (s.current() != '.') {
                    return .OctalTokenWithoutO;
                }
            }
        } else if (first != '.') {
            while ((s.consumeDigit()) or (s.consumeNumericSeparator(DigitType.Digit))) {}
        }
        // we have parsed a 0 or an integer number
        var c = s.current();
        if (c == '.') {
            s.advance();
            if (s.consumeDigit()) {
                while ((s.consumeDigit()) or (s.consumeNumericSeparator(DigitType.Digit))) {}
                c = s.current();
            } else if (first == '.') {
                // number starts with a dot and must be followed by digits
                s.move(-1);
                return .ErrorToken; // may be dot or ellipsis
            } else {
                c = s.current();
            }
        } else if (c == 'n') {
            s.advance();
            return .BigIntToken;
        }
        if (c == 'e' or c == 'E') {
            s.advance();
            c = s.current();
            if (c == '+' or c == '-') {
                s.advance();
            }
            if (!(s.consumeDigit())) {
                if (s.current() == '+' or s.current() == '-') {
                    s.advance();
                    while ((s.consumeDigit()) or (s.consumeNumericSeparator(DigitType.Digit))) {}
                }
                s.addError("invalid number");
                return .ErrorToken;
            }
            while ((s.consumeDigit()) or (s.consumeNumericSeparator(DigitType.Digit))) {}
        }
        return .DecimalToken;
    }

    fn consumeStringToken(s: *Scanner) bool {
        // assume to be on ' or "
        // const mark = s.getMark();
        const delim = s.current();
        s.advance();
        while (true) {
            var c = s.current();
            if (c == delim) {
                s.advance();
                break;
            } else if (c == '\\') {
                s.advance();
                if (!(s.consumeLineTerminator())) {
                    c = s.current();
                    if (c == delim or c == '\\') {
                        s.advance();
                    }
                }
                continue;
            } else if (c == '\n' or c == '\r' or (c == 0 and s.end())) {
                if (c == 0 and s.end()) return false;
                s.advance();
                // s.rewind(mark);
                return false;
            }
            s.advance();
        }
        return true;
    }

    fn consumeRegExpToken(s: *Scanner) bool {
        // assume to be on /
        s.advance();
        var inClass = false;
        while (true) {
            var c = s.current();
            if (!inClass and c == '/') {
                s.advance();
                break;
            } else if (c == '[') {
                inClass = true;
            } else if (c == ']') {
                inClass = false;
            } else if (c == '\\') {
                s.advance();
                if (s.isLineTerminator() or s.current() == 0 and s.errors.items.len == 0) {
                    return false;
                }
            } else if (s.isLineTerminator() or c == 0 and s.errors.items.len == 0) {
                return false;
            }
            s.advance();
        }
        // flags
        while (true) {
            var c = s.current();
            if (identifierTable[c]) {
                s.advance();
            } else if (0xC0 <= c) {
                const rr = s.peekRune();
                if (rr.r == '\u{200C}' or rr.r == '\u{200D}' or isIdentifierContinue(rr.r)) {
                    s.move(rr.n);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return true;
    }

    fn consumeTemplateToken(s: *Scanner) TokenType {
        // assume to be on ` or } when already within template
        var continuation = s.current() == '}';
        s.advance();
        while (true) {
            var c = s.current();
            if (c == '`') {
                // TODO
                // s.templateLevels.items = s.templateLevels.items[0..(s.templateLevels.items.len - 1)];
                const sl = s.templateLevels.toOwnedSlice();
                // const len = sl.len;
                s.templateLevels = ArrayList(usize).fromOwnedSlice(s.internal_allocator, sl[0..(sl.len - 1)]);
                s.advance();
                if (continuation) return .TemplateEndToken;
                return .TemplateToken;
            } else if (c == '$' and s.lookAhead() == '{') {
                s.level += 1;
                s.move(2);
                if (continuation) return .TemplateMiddleToken;
                return .TemplateStartToken;
            } else if (c == '\\') {
                s.advance();
                c = s.current();
                if (c != 0) {
                    s.advance();
                }
                continue;
            } else if (c == 0 and s.errors.items.len == 0) {
                if (!s.end()) {
                    s.advance();
                    continue;
                }
                if (continuation) return .TemplateEndToken;
                return .TemplateToken;
            }
            s.advance();
        }
    }

    fn processRawText(s: *Scanner, start: usize, end_p: usize) void {
        var old_code = s.code;
        var old_cursor = s.cursor;
        var old_start = s.start;
        var old_raw_text_offset = s.raw_text_offset;

        s.raw_text_offset = s.raw_text_offset + s.start;
        s.start = 0;
        s.cursor = 0;
        _ = s.scan(old_code[start..end_p]);
        s.raw_text_offset = old_raw_text_offset;

        s.start = old_start;
        s.cursor = old_cursor;
        s.code = old_code;
    }

    fn addTok(s: *Scanner, tok_type: TokenType, start_pos: usize, end_pos: usize) void {
        const loc = CodeLocation{
            .start = start_pos + s.raw_text_offset,
            .end = end_pos + s.raw_text_offset,
            .start_line = s.start_line,
            .end_line = s.end_line,
            .start_col = s.start_col,
            .end_col = s.end_col,
        };
        s.tokens.append(Token{
            .tok_type = tok_type,
            .loc = loc,
        }) catch unreachable;
    }

    fn parseRawText(s: *Scanner) void {
        var c = s.lookAhead();
        var curlyBracketDepth: usize = 0;
        var string_type: u8 = 0;

        while (!s.end() and (c != '}' or curlyBracketDepth != 0)) {
            if (c == '{' and string_type == 0) {
                curlyBracketDepth += 1;
            }
            if (string_type != 0 and c == '\\') {
                _ = s.advance();
                _ = s.advance();
                c = s.lookAhead();
            }
            if (string_type == c) {
                string_type = 0;
            } else if (string_type == 0 and (c == '"' or c == '\'' or c == '`')) {
                string_type = c;
            }
            _ = s.advance();
            c = s.lookAhead();

            if (c == '}') {
                if (curlyBracketDepth > 0 and string_type == 0) {
                    curlyBracketDepth -= 1;
                } else if (string_type != 0) {
                    _ = s.advance();
                    c = s.lookAhead();
                }
            }
        }
    }

    fn lexeme(s: *Scanner) []const u8 {
        return s.code[s.start..s.cursor];
    }

    fn equalFold(s: *Scanner, str: []const u8, targetLower: []const u8) bool {
        const lxx = s.internal_allocator.alloc(u8, str.len) catch unreachable;
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
                s.internal_allocator.free(lxx);
                return false;
            }
        }
        s.internal_allocator.free(lxx);
        return true;
    }

    fn hexDigit(s: *Scanner) bool {
        const c = s.lookAhead();
        return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' or c <= 'F');
    }

    fn peekRune(s: *Scanner) RuneStruct {
        const pos = s.cursor;
        const c = s.current();
        var r: u21 = rune(c);
        var n: u8 = 1;
        if (c < 192 or s.peek(1) == 0) {
            r = rune(c);
            n = 1;
        } else if (c < 224 or s.peek(2) == 0) {
            r = std.unicode.utf8Decode2(s.code[pos..(pos + 2)]) catch {
                return RuneStruct{ .r = r, .n = n };
            };
            n = 2;
        } else if (c < 240 or s.peek(3) == 0) {
            r = std.unicode.utf8Decode3(s.code[pos..(pos + 3)]) catch {
                return RuneStruct{ .r = r, .n = n };
            };
            n = 3;
        } else {
            r = std.unicode.utf8Decode4(s.code[pos..(pos + 4)]) catch {
                return RuneStruct{ .r = r, .n = n };
            };
            n = 4;
        }

        return RuneStruct{ .r = r, .n = n };
    }

    fn peek(s: *Scanner, n: usize) u8 {
        const pos = n + s.cursor;
        if (s.code.len <= pos) {
            return 0;
        }
        return s.code[pos];
    }

    fn move(p: *Scanner, n: i32) void {
        const newPos = @intCast(usize, @intCast(i32, p.cursor) + n);
        p.cursor = newPos;
    }

    fn current(s: *Scanner) u8 {
        return s.peek(0);
    }

    /// look one character ahead
    fn lookAhead(s: *Scanner) u8 {
        return s.peek(1);
    }

    /// look two characters ahead
    fn lookSuperAhead(s: *Scanner) u8 {
        return s.peek(2);
    }

    fn lookSuperDuperAhead(s: *Scanner) u8 {
        return s.peek(3);
    }

    fn match(s: *Scanner, expectedChar: u8) bool {
        if (s.end()) return false;
        if (s.code[s.cursor] != expectedChar) return false;
        s.*.cursor += 1;
        return true;
    }

    fn advance(s: *Scanner) void {
        if (s.lookAhead() == '\n') {
            s.end_line += 1;
            s.end_col = 0;
        } else {
            s.end_col += 1;
        }
        s.cursor += 1;
    }

    fn rewind(s: *Scanner, mark: usize) void {
        s.cursor = s.start + mark;
    }

    fn getMark(s: *Scanner) usize {
        return s.cursor - s.start;
    }

    fn end(s: *Scanner) bool {
        return s.cursor >= s.code.len;
    }

    pub fn addToken(s: *Scanner, tok_type: TokenType) void {
        s.addTok(tok_type, s.start, s.cursor);
    }

    pub fn addError(s: *Scanner, message: []const u8) void {
        var line: usize = 1;
        var col: usize = 1;
        var i: usize = 0;
        while (i < s.start) : (i += 1) {
            if (s.code[i] == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

        _ = message;

        s.errors.append(Error{
            .line = line,
            .col = col,
            .start = s.start,
            .end = s.cursor,
            .error_type = ParserErrorType.TokenizerError,
        }) catch unreachable;
    }

    // Only for debugging purposes
    pub fn printTokens(p: *Scanner) void {
        std.debug.print("========= TOKENS ===========\nToken length: {d}\n", .{p.tokens.items.len});
        for (p.tokens.items) |tok| {
            const t: Token = tok;
            std.debug.print("{s}\n", .{t.toPrintString(p.internal_allocator, p.code)});
        }
        std.debug.print("====================\n", .{});
    }

    pub fn deinitInternal(s: *Scanner) void {
        s.scanner_arena.deinit();
        s.allocator.destroy(s.scanner_arena);
    }

    pub fn deinit(s: *Scanner) void {
        s.deinitInternal();
    }
};
