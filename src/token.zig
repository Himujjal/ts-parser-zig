const std = @import("std");
const utils = @import("utils.zig");

const Allocator = std.mem.Allocator;

pub const DigitType = enum { Digit, Hex, Binary, Octal };

pub const ContextualKeyword = enum {
    NONE,
    _abstract,
    _accessor,
    _as,
    _assert,
    _asserts,
    _async,
    _await,
    _checks,
    _constructor,
    _declare,
    _enum,
    _exports,
    _from,
    _get,
    _global,
    _implements,
    _infer,
    _interface,
    _is,
    _keyof,
    _mixins,
    _module,
    _namespace,
    _of,
    _opaque,
    _out,
    _override,
    _private,
    _protected,
    _proto,
    _public,
    _readonly,
    _require,
    _set,
    _static,
    _symbol,
    _type,
    _unique,
};

/// Basically an enum of all Possible Token Types
/// Referred from https://github.com/antlr/grammars-v4/blob/master/javascript/typescript/TypeScriptLexer.g4
pub const TokenType = enum {
    /// Misc Tokens
    ErrorToken, // extra token when errors occur
    WhitespaceToken,
    LineTerminatorToken, // \r \n \r\n
    CommentToken,
    CommentLineTerminatorToken,
    StringToken,

    TemplateToken,
    TemplateStartToken,
    TemplateMiddleToken,
    TemplateEndToken,

    RegExpToken,
    PrivateIdentifierToken,

    /// Number like Token Values
    DecimalToken, // all numbers
    BinaryToken, // binary `0b01010101`
    OctalToken, // octal `0o134`
    OctalTokenWithoutO, // octal `0o134`
    HexadecimalToken, // hex `0x123`
    BigIntToken, // `123n13`

    OpenBraceToken, // {
    CloseBraceToken, // }
    OpenParenToken, // (
    CloseParenToken, // )
    OpenBracketToken, // [
    CloseBracketToken, // ]
    DotToken, // .
    SemicolonToken, // ;
    CommaToken, // ,
    QuestionToken, // ?
    ColonToken, // :
    ArrowToken, // =>
    EllipsisToken, // ...

    // Operator Token
    EqToken, // =
    EqEqToken, // ==
    EqEqEqToken, // ===
    NotToken, // !
    NotEqToken, // !=
    NotEqEqToken, // !==
    LtToken, // <
    LtEqToken, // <=
    LtLtToken, // <<
    LtLtEqToken, // <<=
    GtToken, // >
    GtEqToken, // >=
    GtGtToken, // >>
    GtGtEqToken, // >>=
    GtGtGtToken, // >>>
    GtGtGtEqToken, // >>>=
    AddToken, // +
    AddEqToken, // +=
    IncrToken, // ++
    SubToken, // -
    SubEqToken, // -=
    DecrToken, // --
    MulToken, // *
    MulEqToken, // *=
    ExpToken, // **
    ExpEqToken, // **=
    DivToken, // /
    DivEqToken, // /=
    ModToken, // %
    ModEqToken, // %=
    BitAndToken, // &
    BitOrToken, // |
    BitXorToken, // ^
    BitNotToken, // ~
    BitAndEqToken, // &=
    BitOrEqToken, // |=
    BitXorEqToken, // ^=
    AndToken, // &&
    OrToken, // ||
    NullishToken, // ??
    AndEqToken, // &&=
    OrEqToken, // ||=
    NullishEqToken, // ??=
    OptChainToken, // ?.

    //, unused in lexer
    PosToken, // +a
    NegToken, // -a
    PreIncrToken, // ++a
    PreDecrToken, // --a
    PostIncrToken, // a++
    PostDecrToken, // a--

    // Reserved Token
    AwaitToken,
    BreakToken,
    CaseToken,
    CatchToken,
    ClassToken,
    ConstToken,
    ContinueToken,
    DebuggerToken,
    DefaultToken,
    DeleteToken,
    DoToken,
    ElseToken,
    EnumToken,
    ExportToken,
    ExtendsToken,
    FalseToken,
    FinallyToken,
    ForToken,
    FunctionToken,
    IfToken,
    ImportToken,
    InToken,
    InstanceofToken,
    NewToken,
    NullToken,
    ReturnToken,
    SuperToken,
    SwitchToken,
    ThisToken,
    ThrowToken,
    TrueToken,
    TryToken,
    TypeofToken,
    YieldToken,
    VarToken,
    VoidToken,
    WhileToken,
    WithToken,

    // Type Token
    ReadOnlyToken, // 'readonly'
    AnyToken, // 'any'
    NumberToken, // 'number'
    BooleanToken, // 'boolean'
    SymbolToken, // 'Symbol'
    TypeAliasToken, // 'type'
    ConstructorToken, // 'constructor'
    NamespaceToken, // 'namespace'
    RequireToken, // 'require'
    ModuleToken, // 'module'
    StringTypeToken, // 'string'

    DeclareToken, // 'declare'
    AbstractToken, // 'abstract'
    IsToken, // 'is'

    IdentifierToken, // Identifier

    // Identifier Token
    AsToken,
    AsyncToken,
    FromToken,
    GetToken,
    ImplementsToken,
    InterfaceToken,
    LetToken,
    MetaToken,
    OfToken,
    PackageToken,
    PrivateToken,
    ProtectedToken,
    PublicToken,
    SetToken,
    StaticToken,
    TargetToken,

    EOF, // "EOF"

    pub fn toString(tt: TokenType) []const u8 {
        return switch (tt) {
            TokenType.ErrorToken => "Error",
            TokenType.WhitespaceToken => "Whitespace",
            TokenType.LineTerminatorToken => "LineTerminator",
            TokenType.CommentToken => "Comment",
            TokenType.CommentLineTerminatorToken => "CommentLineTerminator",
            TokenType.StringToken => "String",
            TokenType.TemplateToken => "Template",
            TokenType.TemplateStartToken => "TemplateStart",
            TokenType.TemplateMiddleToken => "TemplateMiddle",
            TokenType.TemplateEndToken => "TemplateEnd",
            TokenType.RegExpToken => "RegularExpression",
            TokenType.PrivateIdentifierToken => "PrivateIdentifier",
            TokenType.DecimalToken => "Numeric",
            TokenType.BinaryToken => "Numeric",
            TokenType.OctalToken => "Numeric",
            TokenType.OctalTokenWithoutO => "Numeric",
            TokenType.HexadecimalToken => "Numeric",
            TokenType.BigIntToken => "Numeric",
            TokenType.OpenBraceToken => "Punctuator",
            TokenType.CloseBraceToken => "Punctuator",
            TokenType.OpenParenToken => "Punctuator",
            TokenType.CloseParenToken => "Punctuator",
            TokenType.OpenBracketToken => "Punctuator",
            TokenType.CloseBracketToken => "Punctuator",
            TokenType.DotToken => "Dot",
            TokenType.SemicolonToken => "Punctuator",
            TokenType.CommaToken => "Punctuator",
            TokenType.QuestionToken => "Question",
            TokenType.ColonToken => "Colon",
            TokenType.ArrowToken => "Arrow",
            TokenType.EllipsisToken => "Ellipsis",
            TokenType.EqToken => "Punctuator",
            TokenType.EqEqToken => "EqEq",
            TokenType.EqEqEqToken => "EqEqEq",
            TokenType.NotToken => "Not",
            TokenType.NotEqToken => "NotEq",
            TokenType.NotEqEqToken => "NotEqEq",
            TokenType.LtToken => "Lt",
            TokenType.LtEqToken => "LtEq",
            TokenType.LtLtToken => "LtLt",
            TokenType.LtLtEqToken => "LtLtEq",
            TokenType.GtToken => "Gt",
            TokenType.GtEqToken => "GtEq",
            TokenType.GtGtToken => "GtGt",
            TokenType.GtGtEqToken => "GtGtEq",
            TokenType.GtGtGtToken => "GtGtGt",
            TokenType.GtGtGtEqToken => "GtGtGtEq",
            TokenType.AddToken => "Add",
            TokenType.AddEqToken => "AddEq",
            TokenType.IncrToken => "Incr",
            TokenType.SubToken => "Sub",
            TokenType.SubEqToken => "SubEq",
            TokenType.DecrToken => "Decr",
            TokenType.MulToken => "Mul",
            TokenType.MulEqToken => "MulEq",
            TokenType.ExpToken => "Exp",
            TokenType.ExpEqToken => "ExpEq",
            TokenType.DivToken => "Div",
            TokenType.DivEqToken => "DivEq",
            TokenType.ModToken => "Mod",
            TokenType.ModEqToken => "ModEq",
            TokenType.BitAndToken => "BitAnd",
            TokenType.BitOrToken => "BitOr",
            TokenType.BitXorToken => "BitXor",
            TokenType.BitNotToken => "BitNot",
            TokenType.BitAndEqToken => "BitAndEq",
            TokenType.BitOrEqToken => "BitOrEq",
            TokenType.BitXorEqToken => "BitXorEq",
            TokenType.AndToken => "And",
            TokenType.OrToken => "Or",
            TokenType.NullishToken => "Nullish",
            TokenType.AndEqToken => "AndEq",
            TokenType.OrEqToken => "OrEq",
            TokenType.NullishEqToken => "NullishEq",
            TokenType.OptChainToken => "OptChain",
            TokenType.PosToken => "Pos",
            TokenType.NegToken => "Neg",
            TokenType.PreIncrToken => "PreIncr",
            TokenType.PreDecrToken => "PreDecr",
            TokenType.PostIncrToken => "PostIncr",
            TokenType.PostDecrToken => "PostDecr",
            TokenType.AwaitToken => "Await",
            TokenType.BreakToken => "Break",
            TokenType.CaseToken => "Case",
            TokenType.CatchToken => "Catch",
            TokenType.ClassToken => "Class",
            TokenType.ConstToken => "Const",
            TokenType.ContinueToken => "Continue",
            TokenType.DebuggerToken => "Debugger",
            TokenType.DefaultToken => "Default",
            TokenType.DeleteToken => "Delete",
            TokenType.DoToken => "Do",
            TokenType.ElseToken => "Else",
            TokenType.EnumToken => "Enum",
            TokenType.ExportToken => "Export",
            TokenType.ExtendsToken => "Extends",
            TokenType.FalseToken => "False",
            TokenType.FinallyToken => "Finally",
            TokenType.ForToken => "For",
            TokenType.FunctionToken => "Function",
            TokenType.IfToken => "If",
            TokenType.ImportToken => "Import",
            TokenType.InToken => "In",
            TokenType.InstanceofToken => "Instanceof",
            TokenType.NewToken => "New",
            TokenType.NullToken => "Null",
            TokenType.ReturnToken => "Return",
            TokenType.SuperToken => "Super",
            TokenType.SwitchToken => "Switch",
            TokenType.ThisToken => "This",
            TokenType.ThrowToken => "Throw",
            TokenType.TrueToken => "True",
            TokenType.TryToken => "Try",
            TokenType.TypeofToken => "Typeof",
            TokenType.YieldToken => "Yield",
            TokenType.VarToken => "Var",
            TokenType.VoidToken => "Void",
            TokenType.WhileToken => "While",
            TokenType.WithToken => "With",
            TokenType.ReadOnlyToken => "ReadOnly",
            TokenType.AnyToken => "Any",
            TokenType.NumberToken => "Number",
            TokenType.BooleanToken => "Boolean",
            TokenType.SymbolToken => "Symbol",
            TokenType.TypeAliasToken => "TypeAlias",
            TokenType.ConstructorToken => "Constructor",
            TokenType.NamespaceToken => "Namespace",
            TokenType.RequireToken => "Require",
            TokenType.ModuleToken => "Module",
            TokenType.StringTypeToken => "StringType",
            TokenType.DeclareToken => "Declare",
            TokenType.AbstractToken => "Abstract",
            TokenType.IsToken => "Is",
            TokenType.IdentifierToken => "Identifier",
            TokenType.AsToken => "As",
            TokenType.AsyncToken => "Async",
            TokenType.FromToken => "From",
            TokenType.GetToken => "Get",
            TokenType.ImplementsToken => "Implements",
            TokenType.InterfaceToken => "Interface",
            TokenType.LetToken => "Let",
            TokenType.MetaToken => "Meta",
            TokenType.OfToken => "Of",
            TokenType.PackageToken => "Package",
            TokenType.PrivateToken => "Private",
            TokenType.ProtectedToken => "Protected",
            TokenType.PublicToken => "Public",
            TokenType.SetToken => "Set",
            TokenType.StaticToken => "Static",
            TokenType.TargetToken => "Target",
            else => "",
        };
    }
};

pub const CodeLocation = struct {
    /// Index of the start of the token in the array
    start: usize = 0,

    /// end of the token in the string stream
    end: usize = 0,

    start_line: usize = 1,
    start_col: usize = 0,

    end_line: usize = 1,
    end_col: usize = 0,
};

pub const Token = struct {
    tok_type: TokenType = TokenType.EOF,
    loc: *CodeLocation = undefined,

    pub fn toPrintString(
        self: *const @This(),
        allocator: Allocator,
        code: []const u8,
    ) []const u8 {
        var tok_str = code[self.loc.start..self.loc.end];
        tok_str = switch (self.tok_type) {
            TokenType.ErrorToken => tok_str,
            else => utils.renderStringDecodedUnicode(allocator, tok_str) catch "",
        };

        const res: []const u8 = std.fmt.allocPrint(
            allocator,
            "[\"{s}\", {}, {d}, {d}, {{line.start={d},col.start={d},line.end={d},col.end={d}}}]",
            .{
                tok_str,
                self.tok_type,
                self.loc.start,
                self.loc.end,
                self.loc.start_line,
                self.loc.start_col,
                self.loc.end_line,
                self.loc.end_col,
            },
        ) catch "-----------";
        return res;
    }

    pub fn toString(t: *Token, allocator: Allocator, code: []const u8) []const u8 {
        const tok_str = code[t.loc.start..t.loc.end];
        return switch (t.tok_type) {
            TokenType.ErrorToken => tok_str,
            else => utils.renderStringDecodedUnicode(allocator, tok_str) catch "",
        };
    }

    pub fn testing(self: *Token, allocator: Allocator) void {
        const res: []const u8 = std.fmt.allocPrint(
            allocator,
            "({d},{d})",
            .{ self.start, self.end },
        ) catch "-----------";
        return res;
    }
};

pub fn getTokenTypeFromString(string: []const u8) TokenType {
    if (std.mem.eql(u8, string, "await")) return TokenType.AwaitToken; // 'await';
    if (std.mem.eql(u8, string, "break")) return TokenType.BreakToken; // 'break';
    if (std.mem.eql(u8, string, "do")) return TokenType.DoToken; // 'do';
    if (std.mem.eql(u8, string, "instanceof")) return TokenType.InstanceofToken; // 'instanceof';
    if (std.mem.eql(u8, string, "typeof")) return TokenType.TypeofToken; // 'typeof';
    if (std.mem.eql(u8, string, "case")) return TokenType.CaseToken; // 'case';
    if (std.mem.eql(u8, string, "else")) return TokenType.ElseToken; // 'else';
    if (std.mem.eql(u8, string, "new")) return TokenType.NewToken; // 'new';
    if (std.mem.eql(u8, string, "var")) return TokenType.VarToken; // 'var';
    if (std.mem.eql(u8, string, "catch")) return TokenType.CatchToken; // 'catch';
    if (std.mem.eql(u8, string, "finally")) return TokenType.FinallyToken; // 'finally';
    if (std.mem.eql(u8, string, "return")) return TokenType.ReturnToken; // 'return';
    if (std.mem.eql(u8, string, "void")) return TokenType.VoidToken; // 'void';
    if (std.mem.eql(u8, string, "of")) return TokenType.OfToken; // 'of';
    if (std.mem.eql(u8, string, "target")) return TokenType.TargetToken; // 'Target';
    if (std.mem.eql(u8, string, "continue")) return TokenType.ContinueToken; // 'continue';
    if (std.mem.eql(u8, string, "for")) return TokenType.ForToken; // 'for';
    if (std.mem.eql(u8, string, "switch")) return TokenType.SwitchToken; // 'switch';
    if (std.mem.eql(u8, string, "while")) return TokenType.WhileToken; // 'while';
    if (std.mem.eql(u8, string, "debugger")) return TokenType.DebuggerToken; // 'debugger';
    if (std.mem.eql(u8, string, "function")) return TokenType.FunctionToken; // 'function';
    if (std.mem.eql(u8, string, "this")) return TokenType.ThisToken; // 'this';
    if (std.mem.eql(u8, string, "with")) return TokenType.WithToken; // 'with';
    if (std.mem.eql(u8, string, "default")) return TokenType.DefaultToken; // 'default';
    if (std.mem.eql(u8, string, "if")) return TokenType.IfToken; // 'if';
    if (std.mem.eql(u8, string, "throw")) return TokenType.ThrowToken; // 'throw';
    if (std.mem.eql(u8, string, "delete")) return TokenType.DeleteToken; // 'delete';
    if (std.mem.eql(u8, string, "in")) return TokenType.InToken; // 'in';
    if (std.mem.eql(u8, string, "try")) return TokenType.TryToken; // 'try';
    if (std.mem.eql(u8, string, "as")) return TokenType.AsToken; // 'as';
    if (std.mem.eql(u8, string, "from")) return TokenType.FromToken; // 'from';
    if (std.mem.eql(u8, string, "readonly")) return TokenType.ReadOnlyToken; // 'readonly';
    if (std.mem.eql(u8, string, "async")) return TokenType.AsyncToken; // 'async';
    if (std.mem.eql(u8, string, "class")) return TokenType.ClassToken; // 'class';
    if (std.mem.eql(u8, string, "enum")) return TokenType.EnumToken; // 'enum';
    if (std.mem.eql(u8, string, "extends")) return TokenType.ExtendsToken; // 'extends';
    if (std.mem.eql(u8, string, "super")) return TokenType.SuperToken; // 'super';
    if (std.mem.eql(u8, string, "const")) return TokenType.ConstToken; // 'const';
    if (std.mem.eql(u8, string, "export")) return TokenType.ExportToken; // 'export';
    if (std.mem.eql(u8, string, "import")) return TokenType.ImportToken; // 'import';
    if (std.mem.eql(u8, string, "implements")) return TokenType.ImplementsToken; // 'implements' ;
    if (std.mem.eql(u8, string, "let")) return TokenType.LetToken; // 'let' ;
    if (std.mem.eql(u8, string, "private")) return TokenType.PrivateToken; // 'private' ;
    if (std.mem.eql(u8, string, "public")) return TokenType.PublicToken; // 'public' ;
    if (std.mem.eql(u8, string, "interface")) return TokenType.InterfaceToken; // 'interface' ;
    if (std.mem.eql(u8, string, "package")) return TokenType.PackageToken; // 'package' ;
    if (std.mem.eql(u8, string, "protected")) return TokenType.ProtectedToken; // 'protected' ;
    if (std.mem.eql(u8, string, "static")) return TokenType.StaticToken; // 'static' ;
    if (std.mem.eql(u8, string, "yield")) return TokenType.YieldToken; // 'yield' ;
    if (std.mem.eql(u8, string, "any")) return TokenType.AnyToken; // 'any';
    if (std.mem.eql(u8, string, "number")) return TokenType.NumberToken; //'number';
    if (std.mem.eql(u8, string, "boolean")) return TokenType.BooleanToken; //'boolean';
    if (std.mem.eql(u8, string, "string")) return TokenType.StringToken; //'string';
    if (std.mem.eql(u8, string, "Symbol")) return TokenType.SymbolToken; //'symbol';
    if (std.mem.eql(u8, string, "type")) return TokenType.TypeAliasToken; // 'type'
    if (std.mem.eql(u8, string, "get")) return TokenType.GetToken; // 'get'
    if (std.mem.eql(u8, string, "set")) return TokenType.SetToken; // 'set'
    if (std.mem.eql(u8, string, "constructor")) return TokenType.ConstructorToken; //'constructor';
    if (std.mem.eql(u8, string, "namespace")) return TokenType.NamespaceToken; //'namespace';
    if (std.mem.eql(u8, string, "require")) return TokenType.RequireToken; //'require';
    if (std.mem.eql(u8, string, "module")) return TokenType.ModuleToken; //'module';
    if (std.mem.eql(u8, string, "declare")) return TokenType.DeclareToken; //'declare';
    if (std.mem.eql(u8, string, "abstract")) return TokenType.AbstractToken; //'abstract';
    if (std.mem.eql(u8, string, "is")) return TokenType.IsToken; // 'is'
    if (std.mem.eql(u8, string, "null")) return TokenType.NullToken; // 'null'
    if (std.mem.eql(u8, string, "true")) return TokenType.TrueToken; // 'true'
    if (std.mem.eql(u8, string, "false")) return TokenType.FalseToken; // 'false'
    if (std.mem.eql(u8, string, "meta")) return TokenType.MetaToken; // 'meta'

    return TokenType.IdentifierToken;
}

pub fn getOpTokens(c: u8) TokenType {
    switch (c) {
        '=' => return .EqToken,
        '!' => return .NotToken,
        '<' => return .LtToken,
        '>' => return .GtToken,
        '+' => return .AddToken,
        '-' => return .SubToken,
        '*' => return .MulToken,
        '/' => return .DivToken,
        '%' => return .ModToken,
        '&' => return .BitAndToken,
        '|' => return .BitOrToken,
        '^' => return .BitXorToken,
        '~' => return .BitNotToken,
        '?' => return .QuestionToken,
        else => return .ErrorToken,
    }
}

pub fn getOpEqTokens(c: u8) TokenType {
    switch (c) {
        '=' => return .EqEqToken,
        '!' => return .NotEqToken,
        '<' => return .LtEqToken,
        '>' => return .GtEqToken,
        '+' => return .AddEqToken,
        '-' => return .SubEqToken,
        '*' => return .MulEqToken,
        '/' => return .DivEqToken,
        '%' => return .ModEqToken,
        '&' => return .BitAndEqToken,
        '|' => return .BitOrEqToken,
        '^' => return .BitXorEqToken,
        else => return .ErrorToken,
    }
}

pub fn getOpOpTokens(c: u8) TokenType {
    switch (c) {
        '<' => return .LtLtToken,
        '+' => return .IncrToken,
        '-' => return .DecrToken,
        '*' => return .ExpToken,
        '&' => return .AndToken,
        '|' => return .OrToken,
        '?' => return .NullishToken,
        else => return .ErrorToken,
    }
}

pub fn getOpOpEqTokens(c: u8) TokenType {
    switch (c) {
        '<' => return .LtLtEqToken,
        '*' => return .ExpEqToken,
        '&' => return .AndEqToken,
        '|' => return .OrEqToken,
        '?' => return .NullishEqToken,
        else => return .ErrorToken,
    }
}

pub const identifierStartTable = [256]bool{
    // ASCII
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, true,  false, false, false, // $
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, true, true, true, true, true, true, true, // A, B, C, D, E, F, G
    true, true, true, true, true, true, true, true, // H, I, J, K, L, M, N, O
    true, true, true, true, true, true, true, true, // P, Q, R, S, T, U, V, W
    true, true, true, false, false, false, false, true, // X, Y, Z, _
    false, true, true, true, true, true, true, true, // a, b, c, d, e, f, g
    true, true, true, true, true, true, true, true, // h, i, j, k, l, m, n, o
    true, true, true, true, true, true, true, true, // p, q, r, s, t, u, v, w
    true,  true,  true,  false, false, false, false, false, // x, y, z

    // non-ASCII
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,

    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,

    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,

    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
};

pub const identifierTable = [256]bool{
    // ASCII
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, true,  false, false, false, // $
    false, false, false, false, false, false, false, false,
    true, true, true, true, true, true, true, true, // 0, 1, 2, 3, 4, 5, 6, 7
    true, true, false, false, false, false, false, false, // 8, 9
    false, true, true, true, true, true, true, true, // A, B, C, D, E, F, G
    true, true, true, true, true, true, true, true, // H, I, J, K, L, M, N, O
    true, true, true, true, true, true, true, true, // P, Q, R, S, T, U, V, W
    true, true, true, false, false, false, false, true, // X, Y, Z, _
    false, true, true, true, true, true, true, true, // a, b, c, d, e, f, g
    true, true, true, true, true, true, true, true, // h, i, j, k, l, m, n, o
    true, true, true, true, true, true, true, true, // p, q, r, s, t, u, v, w
    true,  true,  true,  false, false, false, false, false, // x, y, z

    // non-ASCII
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,

    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,

    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,

    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
    false, false, false, false, false, false, false, false,
};

pub const unicodeESNextIdentifierStart = [_]u21{ 65, 90, 97, 122, 170, 170, 181, 181, 186, 186, 192, 214, 216, 246, 248, 705, 710, 721, 736, 740, 748, 748, 750, 750, 880, 884, 886, 887, 890, 893, 895, 895, 902, 902, 904, 906, 908, 908, 910, 929, 931, 1013, 1015, 1153, 1162, 1327, 1329, 1366, 1369, 1369, 1376, 1416, 1488, 1514, 1519, 1522, 1568, 1610, 1646, 1647, 1649, 1747, 1749, 1749, 1765, 1766, 1774, 1775, 1786, 1788, 1791, 1791, 1808, 1808, 1810, 1839, 1869, 1957, 1969, 1969, 1994, 2026, 2036, 2037, 2042, 2042, 2048, 2069, 2074, 2074, 2084, 2084, 2088, 2088, 2112, 2136, 2144, 2154, 2208, 2228, 2230, 2237, 2308, 2361, 2365, 2365, 2384, 2384, 2392, 2401, 2417, 2432, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2482, 2482, 2486, 2489, 2493, 2493, 2510, 2510, 2524, 2525, 2527, 2529, 2544, 2545, 2556, 2556, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2649, 2652, 2654, 2654, 2674, 2676, 2693, 2701, 2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745, 2749, 2749, 2768, 2768, 2784, 2785, 2809, 2809, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866, 2867, 2869, 2873, 2877, 2877, 2908, 2909, 2911, 2913, 2929, 2929, 2947, 2947, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2972, 2972, 2974, 2975, 2979, 2980, 2984, 2986, 2990, 3001, 3024, 3024, 3077, 3084, 3086, 3088, 3090, 3112, 3114, 3129, 3133, 3133, 3160, 3162, 3168, 3169, 3200, 3200, 3205, 3212, 3214, 3216, 3218, 3240, 3242, 3251, 3253, 3257, 3261, 3261, 3294, 3294, 3296, 3297, 3313, 3314, 3333, 3340, 3342, 3344, 3346, 3386, 3389, 3389, 3406, 3406, 3412, 3414, 3423, 3425, 3450, 3455, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526, 3585, 3632, 3634, 3635, 3648, 3654, 3713, 3714, 3716, 3716, 3718, 3722, 3724, 3747, 3749, 3749, 3751, 3760, 3762, 3763, 3773, 3773, 3776, 3780, 3782, 3782, 3804, 3807, 3840, 3840, 3904, 3911, 3913, 3948, 3976, 3980, 4096, 4138, 4159, 4159, 4176, 4181, 4186, 4189, 4193, 4193, 4197, 4198, 4206, 4208, 4213, 4225, 4238, 4238, 4256, 4293, 4295, 4295, 4301, 4301, 4304, 4346, 4348, 4680, 4682, 4685, 4688, 4694, 4696, 4696, 4698, 4701, 4704, 4744, 4746, 4749, 4752, 4784, 4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4822, 4824, 4880, 4882, 4885, 4888, 4954, 4992, 5007, 5024, 5109, 5112, 5117, 5121, 5740, 5743, 5759, 5761, 5786, 5792, 5866, 5870, 5880, 5888, 5900, 5902, 5905, 5920, 5937, 5952, 5969, 5984, 5996, 5998, 6000, 6016, 6067, 6103, 6103, 6108, 6108, 6176, 6264, 6272, 6312, 6314, 6314, 6320, 6389, 6400, 6430, 6480, 6509, 6512, 6516, 6528, 6571, 6576, 6601, 6656, 6678, 6688, 6740, 6823, 6823, 6917, 6963, 6981, 6987, 7043, 7072, 7086, 7087, 7098, 7141, 7168, 7203, 7245, 7247, 7258, 7293, 7296, 7304, 7312, 7354, 7357, 7359, 7401, 7404, 7406, 7411, 7413, 7414, 7418, 7418, 7424, 7615, 7680, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027, 8029, 8029, 8031, 8061, 8064, 8116, 8118, 8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8305, 8305, 8319, 8319, 8336, 8348, 8450, 8450, 8455, 8455, 8458, 8467, 8469, 8469, 8472, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8505, 8508, 8511, 8517, 8521, 8526, 8526, 8544, 8584, 11264, 11310, 11312, 11358, 11360, 11492, 11499, 11502, 11506, 11507, 11520, 11557, 11559, 11559, 11565, 11565, 11568, 11623, 11631, 11631, 11648, 11670, 11680, 11686, 11688, 11694, 11696, 11702, 11704, 11710, 11712, 11718, 11720, 11726, 11728, 11734, 11736, 11742, 12293, 12295, 12321, 12329, 12337, 12341, 12344, 12348, 12353, 12438, 12443, 12447, 12449, 12538, 12540, 12543, 12549, 12591, 12593, 12686, 12704, 12730, 12784, 12799, 13312, 19893, 19968, 40943, 40960, 42124, 42192, 42237, 42240, 42508, 42512, 42527, 42538, 42539, 42560, 42606, 42623, 42653, 42656, 42735, 42775, 42783, 42786, 42888, 42891, 42943, 42946, 42950, 42999, 43009, 43011, 43013, 43015, 43018, 43020, 43042, 43072, 43123, 43138, 43187, 43250, 43255, 43259, 43259, 43261, 43262, 43274, 43301, 43312, 43334, 43360, 43388, 43396, 43442, 43471, 43471, 43488, 43492, 43494, 43503, 43514, 43518, 43520, 43560, 43584, 43586, 43588, 43595, 43616, 43638, 43642, 43642, 43646, 43695, 43697, 43697, 43701, 43702, 43705, 43709, 43712, 43712, 43714, 43714, 43739, 43741, 43744, 43754, 43762, 43764, 43777, 43782, 43785, 43790, 43793, 43798, 43808, 43814, 43816, 43822, 43824, 43866, 43868, 43879, 43888, 44002, 44032, 55203, 55216, 55238, 55243, 55291, 63744, 64109, 64112, 64217, 64256, 64262, 64275, 64279, 64285, 64285, 64287, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320, 64321, 64323, 64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65136, 65140, 65142, 65276, 65313, 65338, 65345, 65370, 65382, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500, 65536, 65547, 65549, 65574, 65576, 65594, 65596, 65597, 65599, 65613, 65616, 65629, 65664, 65786, 65856, 65908, 66176, 66204, 66208, 66256, 66304, 66335, 66349, 66378, 66384, 66421, 66432, 66461, 66464, 66499, 66504, 66511, 66513, 66517, 66560, 66717, 66736, 66771, 66776, 66811, 66816, 66855, 66864, 66915, 67072, 67382, 67392, 67413, 67424, 67431, 67584, 67589, 67592, 67592, 67594, 67637, 67639, 67640, 67644, 67644, 67647, 67669, 67680, 67702, 67712, 67742, 67808, 67826, 67828, 67829, 67840, 67861, 67872, 67897, 67968, 68023, 68030, 68031, 68096, 68096, 68112, 68115, 68117, 68119, 68121, 68149, 68192, 68220, 68224, 68252, 68288, 68295, 68297, 68324, 68352, 68405, 68416, 68437, 68448, 68466, 68480, 68497, 68608, 68680, 68736, 68786, 68800, 68850, 68864, 68899, 69376, 69404, 69415, 69415, 69424, 69445, 69600, 69622, 69635, 69687, 69763, 69807, 69840, 69864, 69891, 69926, 69956, 69956, 69968, 70002, 70006, 70006, 70019, 70066, 70081, 70084, 70106, 70106, 70108, 70108, 70144, 70161, 70163, 70187, 70272, 70278, 70280, 70280, 70282, 70285, 70287, 70301, 70303, 70312, 70320, 70366, 70405, 70412, 70415, 70416, 70419, 70440, 70442, 70448, 70450, 70451, 70453, 70457, 70461, 70461, 70480, 70480, 70493, 70497, 70656, 70708, 70727, 70730, 70751, 70751, 70784, 70831, 70852, 70853, 70855, 70855, 71040, 71086, 71128, 71131, 71168, 71215, 71236, 71236, 71296, 71338, 71352, 71352, 71424, 71450, 71680, 71723, 71840, 71903, 71935, 71935, 72096, 72103, 72106, 72144, 72161, 72161, 72163, 72163, 72192, 72192, 72203, 72242, 72250, 72250, 72272, 72272, 72284, 72329, 72349, 72349, 72384, 72440, 72704, 72712, 72714, 72750, 72768, 72768, 72818, 72847, 72960, 72966, 72968, 72969, 72971, 73008, 73030, 73030, 73056, 73061, 73063, 73064, 73066, 73097, 73112, 73112, 73440, 73458, 73728, 74649, 74752, 74862, 74880, 75075, 77824, 78894, 82944, 83526, 92160, 92728, 92736, 92766, 92880, 92909, 92928, 92975, 92992, 92995, 93027, 93047, 93053, 93071, 93760, 93823, 93952, 94026, 94032, 94032, 94099, 94111, 94176, 94177, 94179, 94179, 94208, 100343, 100352, 101106, 110592, 110878, 110928, 110930, 110948, 110951, 110960, 111355, 113664, 113770, 113776, 113788, 113792, 113800, 113808, 113817, 119808, 119892, 119894, 119964, 119966, 119967, 119970, 119970, 119973, 119974, 119977, 119980, 119982, 119993, 119995, 119995, 119997, 120003, 120005, 120069, 120071, 120074, 120077, 120084, 120086, 120092, 120094, 120121, 120123, 120126, 120128, 120132, 120134, 120134, 120138, 120144, 120146, 120485, 120488, 120512, 120514, 120538, 120540, 120570, 120572, 120596, 120598, 120628, 120630, 120654, 120656, 120686, 120688, 120712, 120714, 120744, 120746, 120770, 120772, 120779, 123136, 123180, 123191, 123197, 123214, 123214, 123584, 123627, 124928, 125124, 125184, 125251, 125259, 125259, 126464, 126467, 126469, 126495, 126497, 126498, 126500, 126500, 126503, 126503, 126505, 126514, 126516, 126519, 126521, 126521, 126523, 126523, 126530, 126530, 126535, 126535, 126537, 126537, 126539, 126539, 126541, 126543, 126545, 126546, 126548, 126548, 126551, 126551, 126553, 126553, 126555, 126555, 126557, 126557, 126559, 126559, 126561, 126562, 126564, 126564, 126567, 126570, 126572, 126578, 126580, 126583, 126585, 126588, 126590, 126590, 126592, 126601, 126603, 126619, 126625, 126627, 126629, 126633, 126635, 126651, 131072, 173782, 173824, 177972, 177984, 178205, 178208, 183969, 183984, 191456, 194560, 195101 };
pub const unicodeESNextIdentifierPart = [_]u21{ 48, 57, 65, 90, 95, 95, 97, 122, 170, 170, 181, 181, 183, 183, 186, 186, 192, 214, 216, 246, 248, 705, 710, 721, 736, 740, 748, 748, 750, 750, 768, 884, 886, 887, 890, 893, 895, 895, 902, 906, 908, 908, 910, 929, 931, 1013, 1015, 1153, 1155, 1159, 1162, 1327, 1329, 1366, 1369, 1369, 1376, 1416, 1425, 1469, 1471, 1471, 1473, 1474, 1476, 1477, 1479, 1479, 1488, 1514, 1519, 1522, 1552, 1562, 1568, 1641, 1646, 1747, 1749, 1756, 1759, 1768, 1770, 1788, 1791, 1791, 1808, 1866, 1869, 1969, 1984, 2037, 2042, 2042, 2045, 2045, 2048, 2093, 2112, 2139, 2144, 2154, 2208, 2228, 2230, 2237, 2259, 2273, 2275, 2403, 2406, 2415, 2417, 2435, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2482, 2482, 2486, 2489, 2492, 2500, 2503, 2504, 2507, 2510, 2519, 2519, 2524, 2525, 2527, 2531, 2534, 2545, 2556, 2556, 2558, 2558, 2561, 2563, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2620, 2620, 2622, 2626, 2631, 2632, 2635, 2637, 2641, 2641, 2649, 2652, 2654, 2654, 2662, 2677, 2689, 2691, 2693, 2701, 2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745, 2748, 2757, 2759, 2761, 2763, 2765, 2768, 2768, 2784, 2787, 2790, 2799, 2809, 2815, 2817, 2819, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866, 2867, 2869, 2873, 2876, 2884, 2887, 2888, 2891, 2893, 2902, 2903, 2908, 2909, 2911, 2915, 2918, 2927, 2929, 2929, 2946, 2947, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2972, 2972, 2974, 2975, 2979, 2980, 2984, 2986, 2990, 3001, 3006, 3010, 3014, 3016, 3018, 3021, 3024, 3024, 3031, 3031, 3046, 3055, 3072, 3084, 3086, 3088, 3090, 3112, 3114, 3129, 3133, 3140, 3142, 3144, 3146, 3149, 3157, 3158, 3160, 3162, 3168, 3171, 3174, 3183, 3200, 3203, 3205, 3212, 3214, 3216, 3218, 3240, 3242, 3251, 3253, 3257, 3260, 3268, 3270, 3272, 3274, 3277, 3285, 3286, 3294, 3294, 3296, 3299, 3302, 3311, 3313, 3314, 3328, 3331, 3333, 3340, 3342, 3344, 3346, 3396, 3398, 3400, 3402, 3406, 3412, 3415, 3423, 3427, 3430, 3439, 3450, 3455, 3458, 3459, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526, 3530, 3530, 3535, 3540, 3542, 3542, 3544, 3551, 3558, 3567, 3570, 3571, 3585, 3642, 3648, 3662, 3664, 3673, 3713, 3714, 3716, 3716, 3718, 3722, 3724, 3747, 3749, 3749, 3751, 3773, 3776, 3780, 3782, 3782, 3784, 3789, 3792, 3801, 3804, 3807, 3840, 3840, 3864, 3865, 3872, 3881, 3893, 3893, 3895, 3895, 3897, 3897, 3902, 3911, 3913, 3948, 3953, 3972, 3974, 3991, 3993, 4028, 4038, 4038, 4096, 4169, 4176, 4253, 4256, 4293, 4295, 4295, 4301, 4301, 4304, 4346, 4348, 4680, 4682, 4685, 4688, 4694, 4696, 4696, 4698, 4701, 4704, 4744, 4746, 4749, 4752, 4784, 4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4822, 4824, 4880, 4882, 4885, 4888, 4954, 4957, 4959, 4969, 4977, 4992, 5007, 5024, 5109, 5112, 5117, 5121, 5740, 5743, 5759, 5761, 5786, 5792, 5866, 5870, 5880, 5888, 5900, 5902, 5908, 5920, 5940, 5952, 5971, 5984, 5996, 5998, 6000, 6002, 6003, 6016, 6099, 6103, 6103, 6108, 6109, 6112, 6121, 6155, 6157, 6160, 6169, 6176, 6264, 6272, 6314, 6320, 6389, 6400, 6430, 6432, 6443, 6448, 6459, 6470, 6509, 6512, 6516, 6528, 6571, 6576, 6601, 6608, 6618, 6656, 6683, 6688, 6750, 6752, 6780, 6783, 6793, 6800, 6809, 6823, 6823, 6832, 6845, 6912, 6987, 6992, 7001, 7019, 7027, 7040, 7155, 7168, 7223, 7232, 7241, 7245, 7293, 7296, 7304, 7312, 7354, 7357, 7359, 7376, 7378, 7380, 7418, 7424, 7673, 7675, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027, 8029, 8029, 8031, 8061, 8064, 8116, 8118, 8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8255, 8256, 8276, 8276, 8305, 8305, 8319, 8319, 8336, 8348, 8400, 8412, 8417, 8417, 8421, 8432, 8450, 8450, 8455, 8455, 8458, 8467, 8469, 8469, 8472, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8505, 8508, 8511, 8517, 8521, 8526, 8526, 8544, 8584, 11264, 11310, 11312, 11358, 11360, 11492, 11499, 11507, 11520, 11557, 11559, 11559, 11565, 11565, 11568, 11623, 11631, 11631, 11647, 11670, 11680, 11686, 11688, 11694, 11696, 11702, 11704, 11710, 11712, 11718, 11720, 11726, 11728, 11734, 11736, 11742, 11744, 11775, 12293, 12295, 12321, 12335, 12337, 12341, 12344, 12348, 12353, 12438, 12441, 12447, 12449, 12538, 12540, 12543, 12549, 12591, 12593, 12686, 12704, 12730, 12784, 12799, 13312, 19893, 19968, 40943, 40960, 42124, 42192, 42237, 42240, 42508, 42512, 42539, 42560, 42607, 42612, 42621, 42623, 42737, 42775, 42783, 42786, 42888, 42891, 42943, 42946, 42950, 42999, 43047, 43072, 43123, 43136, 43205, 43216, 43225, 43232, 43255, 43259, 43259, 43261, 43309, 43312, 43347, 43360, 43388, 43392, 43456, 43471, 43481, 43488, 43518, 43520, 43574, 43584, 43597, 43600, 43609, 43616, 43638, 43642, 43714, 43739, 43741, 43744, 43759, 43762, 43766, 43777, 43782, 43785, 43790, 43793, 43798, 43808, 43814, 43816, 43822, 43824, 43866, 43868, 43879, 43888, 44010, 44012, 44013, 44016, 44025, 44032, 55203, 55216, 55238, 55243, 55291, 63744, 64109, 64112, 64217, 64256, 64262, 64275, 64279, 64285, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320, 64321, 64323, 64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65024, 65039, 65056, 65071, 65075, 65076, 65101, 65103, 65136, 65140, 65142, 65276, 65296, 65305, 65313, 65338, 65343, 65343, 65345, 65370, 65382, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500, 65536, 65547, 65549, 65574, 65576, 65594, 65596, 65597, 65599, 65613, 65616, 65629, 65664, 65786, 65856, 65908, 66045, 66045, 66176, 66204, 66208, 66256, 66272, 66272, 66304, 66335, 66349, 66378, 66384, 66426, 66432, 66461, 66464, 66499, 66504, 66511, 66513, 66517, 66560, 66717, 66720, 66729, 66736, 66771, 66776, 66811, 66816, 66855, 66864, 66915, 67072, 67382, 67392, 67413, 67424, 67431, 67584, 67589, 67592, 67592, 67594, 67637, 67639, 67640, 67644, 67644, 67647, 67669, 67680, 67702, 67712, 67742, 67808, 67826, 67828, 67829, 67840, 67861, 67872, 67897, 67968, 68023, 68030, 68031, 68096, 68099, 68101, 68102, 68108, 68115, 68117, 68119, 68121, 68149, 68152, 68154, 68159, 68159, 68192, 68220, 68224, 68252, 68288, 68295, 68297, 68326, 68352, 68405, 68416, 68437, 68448, 68466, 68480, 68497, 68608, 68680, 68736, 68786, 68800, 68850, 68864, 68903, 68912, 68921, 69376, 69404, 69415, 69415, 69424, 69456, 69600, 69622, 69632, 69702, 69734, 69743, 69759, 69818, 69840, 69864, 69872, 69881, 69888, 69940, 69942, 69951, 69956, 69958, 69968, 70003, 70006, 70006, 70016, 70084, 70089, 70092, 70096, 70106, 70108, 70108, 70144, 70161, 70163, 70199, 70206, 70206, 70272, 70278, 70280, 70280, 70282, 70285, 70287, 70301, 70303, 70312, 70320, 70378, 70384, 70393, 70400, 70403, 70405, 70412, 70415, 70416, 70419, 70440, 70442, 70448, 70450, 70451, 70453, 70457, 70459, 70468, 70471, 70472, 70475, 70477, 70480, 70480, 70487, 70487, 70493, 70499, 70502, 70508, 70512, 70516, 70656, 70730, 70736, 70745, 70750, 70751, 70784, 70853, 70855, 70855, 70864, 70873, 71040, 71093, 71096, 71104, 71128, 71133, 71168, 71232, 71236, 71236, 71248, 71257, 71296, 71352, 71360, 71369, 71424, 71450, 71453, 71467, 71472, 71481, 71680, 71738, 71840, 71913, 71935, 71935, 72096, 72103, 72106, 72151, 72154, 72161, 72163, 72164, 72192, 72254, 72263, 72263, 72272, 72345, 72349, 72349, 72384, 72440, 72704, 72712, 72714, 72758, 72760, 72768, 72784, 72793, 72818, 72847, 72850, 72871, 72873, 72886, 72960, 72966, 72968, 72969, 72971, 73014, 73018, 73018, 73020, 73021, 73023, 73031, 73040, 73049, 73056, 73061, 73063, 73064, 73066, 73102, 73104, 73105, 73107, 73112, 73120, 73129, 73440, 73462, 73728, 74649, 74752, 74862, 74880, 75075, 77824, 78894, 82944, 83526, 92160, 92728, 92736, 92766, 92768, 92777, 92880, 92909, 92912, 92916, 92928, 92982, 92992, 92995, 93008, 93017, 93027, 93047, 93053, 93071, 93760, 93823, 93952, 94026, 94031, 94087, 94095, 94111, 94176, 94177, 94179, 94179, 94208, 100343, 100352, 101106, 110592, 110878, 110928, 110930, 110948, 110951, 110960, 111355, 113664, 113770, 113776, 113788, 113792, 113800, 113808, 113817, 113821, 113822, 119141, 119145, 119149, 119154, 119163, 119170, 119173, 119179, 119210, 119213, 119362, 119364, 119808, 119892, 119894, 119964, 119966, 119967, 119970, 119970, 119973, 119974, 119977, 119980, 119982, 119993, 119995, 119995, 119997, 120003, 120005, 120069, 120071, 120074, 120077, 120084, 120086, 120092, 120094, 120121, 120123, 120126, 120128, 120132, 120134, 120134, 120138, 120144, 120146, 120485, 120488, 120512, 120514, 120538, 120540, 120570, 120572, 120596, 120598, 120628, 120630, 120654, 120656, 120686, 120688, 120712, 120714, 120744, 120746, 120770, 120772, 120779, 120782, 120831, 121344, 121398, 121403, 121452, 121461, 121461, 121476, 121476, 121499, 121503, 121505, 121519, 122880, 122886, 122888, 122904, 122907, 122913, 122915, 122916, 122918, 122922, 123136, 123180, 123184, 123197, 123200, 123209, 123214, 123214, 123584, 123641, 124928, 125124, 125136, 125142, 125184, 125259, 125264, 125273, 126464, 126467, 126469, 126495, 126497, 126498, 126500, 126500, 126503, 126503, 126505, 126514, 126516, 126519, 126521, 126521, 126523, 126523, 126530, 126530, 126535, 126535, 126537, 126537, 126539, 126539, 126541, 126543, 126545, 126546, 126548, 126548, 126551, 126551, 126553, 126553, 126555, 126555, 126557, 126557, 126559, 126559, 126561, 126562, 126564, 126564, 126567, 126570, 126572, 126578, 126580, 126583, 126585, 126588, 126590, 126590, 126592, 126601, 126603, 126619, 126625, 126627, 126629, 126633, 126635, 126651, 131072, 173782, 173824, 177972, 177984, 178205, 178208, 183969, 183984, 191456, 194560, 195101, 917760, 917999 };
pub const WhiteSpaceUnicode = [_]u21{
    9, // \t
    12, // \f
    32, // ' ' | <space>
    160, // <NBSP>
    11135, // \v
    65279, // <ZWNBSP>
    8192, // En QUad
    8193,
    8194,
    8196,
    8199,
    8200,
    8201,
    8202,
};
pub const LineTermUnicode = [_]u21{
    10, // \n
    13, // \r
    8232, // <LS>
    8233, // <PS>
};
