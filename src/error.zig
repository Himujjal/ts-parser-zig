const std = @import("std");

pub const ParserErrorType = error{
    TokenizerError, // only for dev purposes
    MissingSemiColon,
    EOFError,
};

pub const Error = struct {
    line: usize,
    startPosition: usize,
    endPosition: usize,
    errorMessage: []const u8,
    errorType: ParserErrorType,
};
