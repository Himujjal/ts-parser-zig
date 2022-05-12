const std = @import("std");
const scanner = @import("./scanner.zig");
const token = @import("./token.zig");
const _error = @import("./error.zig");

const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;

const Scanner = scanner.Scanner;

const Token = token.Token;
const TokenType = scanner.TokenType;

const Error = _error.Error;
const ParserErrorType = _error.ParserErrorType;

pub const SvelteScriptLanguage = enum { JS, Dart, TypeScript };
pub const SvelteStyleLanguage = enum { CSS, SCSS, POSTCSS };

pub const ParserOptions = struct {};

pub const Tree = struct {
    const Self = @This();

    rootNode: struct {},

    pub fn toString(self: *Tree) []const u8 {
        _ = self;
        return 
        \\Div
        \\  Children
        \\      Text
        \\          Hello Svelte
        ;
    }
};

pub const Parser = struct {
    const Self = @This();

    allocator: Allocator,
    tree: Tree,
    scannerInstance: Scanner,

    code: []const u8 = undefined,
    errors: *ArrayList(Error),
    warnings: *ArrayList(Error),
    tokens: *ArrayList(Token),
    options: ParserOptions,

    parser_arena: *ArenaAllocator,
    internal_allocator: Allocator,

    pub fn init(allocator: Allocator, options: ParserOptions) Self {
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

        return Self{
            .allocator = allocator,
            .scannerInstance = scannerInstance,
            .tree = undefined,
            .errors = errors,
            .warnings = warnings,
            .options = options,
            .tokens = tokens,

            .parser_arena = parser_arena,
            .internal_allocator = parser_arena.allocator(),
        };
    }

    pub fn parse(self: *Self, code: []const u8) *Self {
        const s = self.scannerInstance.scan(code);
        _ = s;
        // s.printTokens();
        return self;
    }

    pub fn deinitInternal(self: *Self) void {
        self.parser_arena.deinit();
        self.allocator.destroy(self.parser_arena);
        self.allocator.destroy(self.errors);
        self.allocator.destroy(self.warnings);
        self.allocator.destroy(self.tokens);
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.errors.deinit();
        self.warnings.deinit();
        self.deinitInternal();
        self.scannerInstance.deinit();
    }
};
