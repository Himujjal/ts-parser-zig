const std = @import("std");

const Allocator = std.mem.Allocator;

/// Basically an enum of all Possible Token Types
/// Referred from https://github.com/antlr/grammars-v4/blob/master/javascript/typescript/TypeScriptLexer.g4
pub const TokenType = enum {
    // --- Operators and symbold ---
    MutliLineComment, // /* */
    SingleLineComment, // //
    RegularExpressionLIteral, // /[...]/
    OpenBracket, // [
    CloseBracket, // ]
    OpenParen, // "("
    CloseParen, // ")"
    OpenBrace, // "}"
    CloseBrace, // "{"
    SemiColon, // ';'
    Comma, // ','
    Assign, // '='
    Question, // '?'
    QuestionQuestion, // '??'
    QuestionQuestionAssign, // '??='
    Colon, // ':'
    Ellipsis, // '...'
    Dot, // '.'
    PlusPlus, // '++'
    MinusMinus, // '--'
    Plus, // '+'
    Minus, // '-'
    BitNot, // '~';
    Not, // '!'
    Multiply, // '*'
    MultiplyAssign, // '*='
    Exponent, // '**'
    ExponentAssign, // '**='
    Divide, // '/'
    Modulus, // '%';
    LessThan, // '<'
    LessThanEquals, // '<='
    LeftShiftArithmeticAssign, // '<<='
    LeftShiftArithmetic, // '<<';
    GreaterThan, // '>'
    GreaterThanEquals, // '>='
    RightShiftArithmetic, // '>>';
    RightShiftArithmeticAssign, // '>>='
    RightShiftLogical, // '>>>';
    RightShiftLogicalAssign, // '>>>='
    Equals, // '=='
    NotEquals, // '!='
    IdentityEquals, // '==='
    IdentityNotEquals, // '!=='
    BitAnd, // '&'
    BitAndAssign, // '&='
    And, // '&&'
    AndAssign, // '&&='
    BitOr, // '|'
    BitOrAssign, // '|='
    Or, // '||'
    OrAssign, // '||='
    BitXor, // '^'
    BitXorAssign, // '^='
    DivideAssign, // '/='
    ModulusAssign, // '%='
    PlusAssign, // '+='
    MinusAssign, // '-='
    Arrow, // '=>'

    // -- Literals --
    NullLiteral, // 'null'
    TrueLiteral, // 'true'
    FalseLiteral, // 'false'
    NumberLiteral, // includes all binary, octal, hexadecimal, underscored stuff

    // --- Keywords --

    Break, // 'break';
    Do, // 'do';
    Instanceof, // 'instanceof';
    Typeof, // 'typeof';
    Case, // 'case';
    Else, // 'else';
    New, // 'new';
    Var, // 'var';
    Catch, // 'catch';
    Finally, // 'finally';
    Return, // 'return';
    Void, // 'void';
    Continue, // 'continue';
    For, // 'for';
    Switch, // 'switch';
    While, // 'while';
    Debugger, // 'debugger';
    Function, // 'function';
    This, // 'this';
    With, // 'with';
    Default, // 'default';
    If, // 'if';
    Throw, // 'throw';
    Delete, // 'delete';
    In, // 'in';
    Try, // 'try';
    As, // 'as';
    From, // 'from';
    ReadOnly, // 'readonly';
    Async, // 'async';
    Class, // 'class';
    Enum, // 'enum';
    Extends, // 'extends';
    Super, // 'super';
    Const, // 'const';
    Export, // 'export';
    Import, // 'import';
    Implements, // 'implements' ;
    Let, // 'let' ;
    Private, // 'private' ;
    Public, // 'public' ;
    Interface, // 'interface' ;
    Package, // 'package' ;
    Protected, // 'protected' ;
    Static, // 'static' ;
    Yield, // 'yield' ;
    Any, // 'any';
    Number, //'number';
    Boolean, //'boolean';
    String, //'string';
    Symbol, //'symbol';
    TypeAlias, // 'type'
    Get, // 'get'
    Set, // 'set'
    Constructor, //'constructor';
    Namespace, //'namespace';
    Require, //'require';
    Module, //'module';
    Declare, //'declare';
    Abstract, //'abstract';
    Is, // 'is'

    At, // '@',
    Identifier, // Any identifier
    StringLiteral, // Double quoted and single quoted
    TemplateLiteral, // "`  `"

    // Dummy token type
    TemplateLiteralStart,
    TemplateLiteralString,
    TemplateLiteralRawText,
    TemplateLiteralEnd,

    EOF, // "EOF"
};

pub const Token = struct {
    const Self = @This();

    tok_type: TokenType = TokenType.EOF,

    /// Index of the start of the token in the array
    start: usize = 0,

    /// end of the token in the string stream
    end: usize = 0,

    pub fn toString(
        self: *const @This(),
        allocator: Allocator,
        code: []const u8,
    ) []const u8 {
        var res: []const u8 = std.fmt.allocPrint(
            allocator,
            "[\"{s}\", {s}, {d}, {d}]",
            .{
                code[self.start..self.end],
                self.tok_type,
                self.start,
                self.end,
            },
        ) catch "-----------";
        return res;
    }

    pub fn testing(self: *Self, allocator: Allocator) void {
        var res: []const u8 = std.fmt.allocPrint(
            allocator,
            "({d},{d})",
            .{ self.start, self.end },
        ) catch "-----------";
        return res;
    }
};

pub fn getTokenTypeFromString(string: []const u8) TokenType {
    if (std.mem.eql(u8, string, "break")) return TokenType.Break; // 'break';
    if (std.mem.eql(u8, string, "do")) return TokenType.Do; // 'do';
    if (std.mem.eql(u8, string, "instanceof")) return TokenType.Instanceof; // 'instanceof';
    if (std.mem.eql(u8, string, "typeof")) return TokenType.Typeof; // 'typeof';
    if (std.mem.eql(u8, string, "case")) return TokenType.Case; // 'case';
    if (std.mem.eql(u8, string, "else")) return TokenType.Else; // 'else';
    if (std.mem.eql(u8, string, "new")) return TokenType.New; // 'new';
    if (std.mem.eql(u8, string, "var")) return TokenType.Var; // 'var';
    if (std.mem.eql(u8, string, "catch")) return TokenType.Catch; // 'catch';
    if (std.mem.eql(u8, string, "finally")) return TokenType.Finally; // 'finally';
    if (std.mem.eql(u8, string, "return")) return TokenType.Return; // 'return';
    if (std.mem.eql(u8, string, "void")) return TokenType.Void; // 'void';
    if (std.mem.eql(u8, string, "continue")) return TokenType.Continue; // 'continue';
    if (std.mem.eql(u8, string, "for")) return TokenType.For; // 'for';
    if (std.mem.eql(u8, string, "switch")) return TokenType.Switch; // 'switch';
    if (std.mem.eql(u8, string, "while")) return TokenType.While; // 'while';
    if (std.mem.eql(u8, string, "debugger")) return TokenType.Debugger; // 'debugger';
    if (std.mem.eql(u8, string, "function")) return TokenType.Function; // 'function';
    if (std.mem.eql(u8, string, "this")) return TokenType.This; // 'this';
    if (std.mem.eql(u8, string, "with")) return TokenType.With; // 'with';
    if (std.mem.eql(u8, string, "default")) return TokenType.Default; // 'default';
    if (std.mem.eql(u8, string, "if")) return TokenType.If; // 'if';
    if (std.mem.eql(u8, string, "throw")) return TokenType.Throw; // 'throw';
    if (std.mem.eql(u8, string, "delete")) return TokenType.Delete; // 'delete';
    if (std.mem.eql(u8, string, "in")) return TokenType.In; // 'in';
    if (std.mem.eql(u8, string, "try")) return TokenType.Try; // 'try';
    if (std.mem.eql(u8, string, "as")) return TokenType.As; // 'as';
    if (std.mem.eql(u8, string, "from")) return TokenType.From; // 'from';
    if (std.mem.eql(u8, string, "readonly")) return TokenType.ReadOnly; // 'readonly';
    if (std.mem.eql(u8, string, "async")) return TokenType.Async; // 'async';
    if (std.mem.eql(u8, string, "class")) return TokenType.Class; // 'class';
    if (std.mem.eql(u8, string, "enum")) return TokenType.Enum; // 'enum';
    if (std.mem.eql(u8, string, "extends")) return TokenType.Extends; // 'extends';
    if (std.mem.eql(u8, string, "super")) return TokenType.Super; // 'super';
    if (std.mem.eql(u8, string, "const")) return TokenType.Const; // 'const';
    if (std.mem.eql(u8, string, "export")) return TokenType.Export; // 'export';
    if (std.mem.eql(u8, string, "import")) return TokenType.Import; // 'import';
    if (std.mem.eql(u8, string, "implements")) return TokenType.Implements; // 'implements' ;
    if (std.mem.eql(u8, string, "let")) return TokenType.Let; // 'let' ;
    if (std.mem.eql(u8, string, "private")) return TokenType.Private; // 'private' ;
    if (std.mem.eql(u8, string, "public")) return TokenType.Public; // 'public' ;
    if (std.mem.eql(u8, string, "interface")) return TokenType.Interface; // 'interface' ;
    if (std.mem.eql(u8, string, "package")) return TokenType.Package; // 'package' ;
    if (std.mem.eql(u8, string, "protected")) return TokenType.Protected; // 'protected' ;
    if (std.mem.eql(u8, string, "static")) return TokenType.Static; // 'static' ;
    if (std.mem.eql(u8, string, "yield")) return TokenType.Yield; // 'yield' ;
    if (std.mem.eql(u8, string, "any")) return TokenType.Any; // 'any';
    if (std.mem.eql(u8, string, "number")) return TokenType.Number; //'number';
    if (std.mem.eql(u8, string, "boolean")) return TokenType.Boolean; //'boolean';
    if (std.mem.eql(u8, string, "string")) return TokenType.String; //'string';
    if (std.mem.eql(u8, string, "Symbol")) return TokenType.Symbol; //'symbol';
    if (std.mem.eql(u8, string, "type")) return TokenType.TypeAlias; // 'type'
    if (std.mem.eql(u8, string, "get")) return TokenType.Get; // 'get'
    if (std.mem.eql(u8, string, "set")) return TokenType.Set; // 'set'
    if (std.mem.eql(u8, string, "constructor")) return TokenType.Constructor; //'constructor';
    if (std.mem.eql(u8, string, "namespace")) return TokenType.Namespace; //'namespace';
    if (std.mem.eql(u8, string, "require")) return TokenType.Require; //'require';
    if (std.mem.eql(u8, string, "module")) return TokenType.Module; //'module';
    if (std.mem.eql(u8, string, "declare")) return TokenType.Declare; //'declare';
    if (std.mem.eql(u8, string, "abstract")) return TokenType.Abstract; //'abstract';
    if (std.mem.eql(u8, string, "is")) return TokenType.Is; // 'is'
    if (std.mem.eql(u8, string, "null")) return TokenType.NullLiteral; // 'null'
    if (std.mem.eql(u8, string, "true")) return TokenType.TrueLiteral; // 'true'
    if (std.mem.eql(u8, string, "false")) return TokenType.FalseLiteral; // 'false'

    return TokenType.Identifier;
}
