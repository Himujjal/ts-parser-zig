const std = @import("std");

pub const ParserErrorType = enum {
    TokenizerError, // only for dev purposes
    MissingSemiColon,
};

pub const Error = struct {
    line: usize,
    startPosition: usize,
    endPosition: usize,
    errorMessage: []const u8,
    errorType: ParserErrorType,
};
