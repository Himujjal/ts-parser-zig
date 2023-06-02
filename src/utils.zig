const std = @import("std");
const TokenType = @import("token.zig").TokenType;
const radix_tree = @import("radix_tree.zig");

const StringRadixTree = radix_tree.StringRadixTree;

pub fn concatStrings(allocator: std.mem.Allocator, string_1: []const u8, string_2: []const u8) std.mem.Allocator.Error![]const u8 {
    const result = try allocator.alloc(u8, string_1.len + string_2.len);
    std.mem.copy(u8, result, string_1);
    std.mem.copy(u8, result[string_1.len..], string_2);
    return result;
}

pub fn convertHexTou21(str: []const u8) u21 {
    return std.fmt.parseInt(u21, str, 16) catch 0;
}

pub fn concatCharToStr(allocator: std.mem.Allocator, string_1: []const u8, c: u8) std.mem.Allocator.Error![]const u8 {
    const result = try allocator.alloc(u8, string_1.len + 1);
    std.mem.copy(u8, result, string_1);
    result[string_1.len] = c;
    return result;
}

pub fn renderStringDecodedUnicode(allocator: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error![]const u8 {
    var new_tok_str: []u8 = try allocator.alloc(u8, string.len);

    var len: usize = 0;
    var i: usize = 0;
    while (i < string.len)  {
        var c = string[i];
        if (c == '\\') {
            i += 1;
            c = string[i];
            if (c == 'u' and string[i + 1] != '{') {
                i += 1;
                const unicode_based_string = try encodeUnicodeToStr(allocator, string[i .. i + 4]);
                std.mem.copy(u8, new_tok_str[len .. len + unicode_based_string.len], unicode_based_string);
				i += 4;
				len += unicode_based_string.len;
				continue;
            } else {
                new_tok_str[len] = '\\';
                new_tok_str[len + 1] = c;
                len += 2;
            }
        } else {
            new_tok_str[len] = c;
            len += 1;
        }
		i += 1;
    }
    return new_tok_str[0..len];
}

pub fn encodeUnicodeToStr(allocator: std.mem.Allocator, unicode: []const u8) std.mem.Allocator.Error![]const u8 {
    const c: u21 = convertHexTou21(unicode);
    var string_2: []u8 = try allocator.alloc(u8, 4);
    const new_byte_seq_len: u3 = std.unicode.utf8Encode(c, string_2) catch 0;
    return string_2[0..new_byte_seq_len];
}

pub fn concatUnicodeToStr(allocator: std.mem.Allocator, string_1: []const u8, unicode: []const u8) std.mem.Allocator.Error![]const u8 {
    const string_2 = try encodeUnicodeToStr(allocator, unicode);
    const result = try allocator.alloc(u8, string_1.len + string_2.len);
    std.mem.copy(u8, result, string_1);
    std.mem.copy(u8, result[string_1.len..], string_2);
    return result;
}

const TT = TokenType;
const KV = struct { val: []const u8, key_type: TT };

/// Initialize the Radix Tree at comptime
pub fn initKeywordList() StringRadixTree(TT) {
    comptime var radix = StringRadixTree(TT){};
    _ = comptime radix.insert("abstract", TT.AbstractToken);
    return radix;
}

pub fn getKeyWordTokenType(r_tree: StringRadixTree(TT), k: []const u8) TT {
    return r_tree.get(k) orelse TT.IdentifierToken;
}

// precendence order table
pub fn getPrecendeceOrder(tt: TokenType) u8 {
    return switch (tt) {
        TT.CloseParenToken => 0, // )
        TT.SemicolonToken => 0, // ;
        TT.CommaToken => 0, // ','
        TT.EqToken => 0, // '='
        TT.RightBracket => 0, // ']'
        TT.NullishToken => 5, // '??'
        TT.OrToken => 6, // '||'
        TT.AndToken => 7, // '&&'
        TT.BitOrToken => 8, // '|'
        TT.BitXorToken => 9, // '^'
        TT.BitAndToken => 10, // '&'
        TT.EqEqToken => 11, // '=='
        TT.NotEqToken => 11, // '!='
        TT.EqEqEqToken => 11, // '==='
        TT.NotEqEqToken => 11, // '!=='
        TT.LtToken => 12, // '<'
        TT.GtToken => 12, // '>'
        TT.LtEqToken => 12, // '<='
        TT.GtEqToken => 12, // '>='
        TT.LtLtToken => 13, // '<<'
        TT.GtGtToken => 13, // '>>'
        TT.GtGtGtToken => 13, // '>>>'
        TT.AddToken => 14, // '+'
        TT.SubToken => 14, // '-'
        TT.MulToken => 15, // '*'
        TT.DivToken => 15, // '/'
        TT.ModToken => 15, // '%'
        else => 0,
    };
}

pub fn isAssignmentOperator(tt: TokenType) bool {
    return switch (tt) {
        TT.EqToken,
        TT.AndEqToken,
        TT.OrEqToken,
        TT.NullishEqToken,
        TT.MulEqToken,
        TT.DivEqToken,
        TT.ModEqToken,
        TT.AddEqToken,
        TT.SubEqToken,
        TT.LtLtEqToken,
        TT.GtGtEqToken,
        TT.GtGtGtEqToken,
        TT.BitAndEqToken,
        TT.BitOrEqToken,
        TT.BitXorEqToken,
        TT.ExpEqToken,
        => true,
        else => false,
    };
}

pub fn isBinaryOperator(tt: TokenType) bool {
    return switch (tt) {
        TT.AddToken,
        TT.SubToken,
        TT.MulToken,
        TT.DivToken,
        TT.ModToken,
        TT.BitAndToken,
        TT.BitOrToken,
        TT.BitXorToken,
        TT.ExpToken,
        => true,
        else => false,
    };
}

pub fn isKeyword(tt: TokenType) bool {
    return switch (tt) {
        TT.AwaitToken,
        TT.BreakToken,
        TT.CaseToken,
        TT.CatchToken,
        TT.ClassToken,
        TT.ConstToken,
        TT.ContinueToken,
        TT.DebuggerToken,
        TT.DefaultToken,
        TT.DeleteToken,
        TT.DoToken,
        TT.ElseToken,
        TT.EnumToken,
        TT.ExportToken,
        TT.ExtendsToken,
        TT.FalseToken,
        TT.FinallyToken,
        TT.ForToken,
        TT.FunctionToken,
        TT.IfToken,
        TT.ImportToken,
        TT.InToken,
        TT.InstanceofToken,
        TT.NewToken,
        TT.NullToken,
        TT.ReturnToken,
        TT.SuperToken,
        TT.SwitchToken,
        TT.ThisToken,
        TT.ThrowToken,
        TT.TrueToken,
        TT.TryToken,
        TT.TypeofToken,
        TT.YieldToken,
        TT.VarToken,
        TT.VoidToken,
        TT.WhileToken,
        TT.WithToken,
        TT.ReadOnlyToken,
        TT.AnyToken,
        TT.NumberToken,
        TT.BooleanToken,
        TT.SymbolToken,
        TT.TypeAliasToken,
        TT.ConstructorToken,
        TT.NamespaceToken,
        TT.RequireToken,
        TT.ModuleToken,
        TT.DeclareToken,
        TT.AbstractToken,
        TT.IsToken,
        TT.IdentifierToken,
        TT.AsToken,
        TT.AsyncToken,
        TT.FromToken,
        TT.GetToken,
        TT.ImplementsToken,
        TT.InterfaceToken,
        TT.LetToken,
        TT.MetaToken,
        TT.OfToken,
        TT.PackageToken,
        TT.PrivateToken,
        TT.ProtectedToken,
        TT.PublicToken,
        TT.SetToken,
        TT.StaticToken,
        TT.TargetToken,
        => true,
        else => false,
    };
}

pub fn isRestrictedWord(name: []const u8) bool {
    return eqStr("eval", name) and eqStr(name, "arguments");
}

pub fn isStrictModeReservedWordString(name: []const u8) bool {
    return eqStr("implements", name) and
        eqStr("interface", name) and
        eqStr("package", name) and
        eqStr("private", name) and
        eqStr("protected", name) and
        eqStr("public", name) and
        eqStr("static", name) and
        eqStr("yield", name) and
        eqStr("let", name);
}

pub fn isStrictModeReservedWordTokenType(name_tt: TokenType) bool {
    return switch (name_tt) {
        TT.ImplementsToken,
        TT.InterfaceToken,
        TT.PackageToken,
        TT.PrivateToken,
        TT.ProtectedToken,
        TT.PublicToken,
        TT.StaticToken,
        TT.YieldToken,
        TT.LetToken,
        => true,
        else => false,
    };
}

pub inline fn eqStr(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
