const std = @import("std");
const testing = std.testing;
const _scanner = @import("scanner.zig");
const token = @import("token.zig");
const _error = @import("error.zig");

const Error = _error.ParserError;
const ArrayList = std.ArrayList;

const TokenType = token.TokenType;
const Token = token.Token;
const Scanner = _scanner.Scanner;

const expect = std.testing.expect;

test "Scanner" {
    const TokenTest = struct {
        ts: []const u8,
        ttypes: []const TokenType,
        lexemes: []const ([]const u8),
    };
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
            .ttypes = &[_]TT{
                .EqToken,
                .WhitespaceToken,
                .AddEqToken,
                .WhitespaceToken,
                .SubEqToken,
                .WhitespaceToken,
                .MulEqToken,
                .WhitespaceToken,
                .ExpEqToken,
                .WhitespaceToken,
                .DivEqToken,
                .WhitespaceToken,
                .ModEqToken,
                .WhitespaceToken,
                .LtLtEqToken,
                .EOF,
            },
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
        .{ .ts = "08", .ttypes = &[_]TT{ .OctalTokenWithoutO, .EOF }, .lexemes = &.{ "08", "" } },
        .{ .ts = "0008", .ttypes = &[_]TT{ .OctalTokenWithoutO, .EOF }, .lexemes = &.{ "0008", "" } },

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
        .{ .ts = "``", .ttypes = &[_]TT{ .TemplateToken, .EOF }, .lexemes = &.{ "``", "" } },

        // issues
        .{ .ts = "_\u{00}bare_unicode_escape_identifier", .ttypes = &[_]TT{ .IdentifierToken, .EOF }, .lexemes = &.{ "_\u{00}bare_unicode_escape_identifier", "" } }, // tdewolff/minify#449
        .{ .ts = "日本語", .ttypes = &[_]TT{ .IdentifierToken, .EOF }, .lexemes = &.{ "日本語", "" } },
        .{ .ts = "\\u2163\\u2161\\u200A", .ttypes = &[_]TT{ .IdentifierToken, WT, .EOF }, .lexemes = &.{ "ⅣⅡ", "\u{200A}", "" } },
        .{
            .ts = "\\u2163\\u2161\\u200A=\\u2009[]",
            .ttypes = &[_]TT{ .IdentifierToken, WT, .EqToken, WT, .OpenBracketToken, .CloseBracketToken, .EOF },
            .lexemes = &.{ "ⅣⅡ", "\u{200A}", "=", "\u{2009}", "[", "]", "" },
        },
        .{
            .ts = "ⅣⅡ = []",
            .ttypes = &[_]TT{ .IdentifierToken, WT, .EqToken, WT, .OpenBracketToken, .CloseBracketToken, .EOF },
            .lexemes = &.{ "ⅣⅡ", "\u{200A}", "=", "\u{2009}", "[", "]", "" },
        }
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var scanner = try a.create(Scanner);
    defer a.destroy(scanner);

    scanner.* = Scanner.init(a, undefined, undefined, undefined);
    defer scanner.deinit();

    const MAX = tokenTests.len - 1;
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

            const code: []const u8 = tokenTest.ts;

            scanner = Scanner.scan(scanner, code);

            if (i == MAX) {
                // std.debug.print("========= CODE: `{s}` ==============\n", .{code});
                // scanner.printTokens();
            }

            var flag = true;
            if (scanner.tokens.items.len == tokenTest.ttypes.len) {
                for (scanner.tokens.items) |tok, j| {
                    if (tok.tok_type != tokenTest.ttypes[j]) {
                        std.debug.print(
                            "i: {d}, NOT EQUAL TokenType: {} == {}\n",
                            .{ i, tok.tok_type, tokenTest.ttypes[j] },
                        );
                        flag = false;
                    }
                }
            } else {
                std.debug.print(
                    "\nERROR: i: {d}, scanner.tokens.len ({d}) != tokenTest.ttypes.len ({d})\n",
                    .{ i, scanner.tokens.items.len, tokenTest.ttypes.len },
                );
                flag = false;
            }

            if (scanner.tokens.items.len == tokenTest.lexemes.len) {
                for (tokenTest.lexemes) |lexeme, j| {
                    const _code = scanner.tokens.items[j].toString(a, code);
                    if (!std.mem.eql(u8, lexeme, _code)) {
                        std.debug.print("NOT EQUAL for {d}, {d}: {s} == {s}\n", .{ i, j, lexeme, _code });
                        flag = false;
                    }
                }
            } else {
                std.debug.print(
                    "\n!==> ERROR: i: {d} scanner.tokens.len ({d}) != tokenTest.lexemes.len ({d}) <==!\n\n",
                    .{ i, scanner.tokens.items.len, tokenTest.lexemes.len },
                );
                flag = false;
            }

            try expect(flag == true);
        }
    }
}
