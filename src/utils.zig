const std = @import("std");

const ArrayList = std.ArrayList;

pub fn sliceArrayList(comptime T: type, arr: ArrayList(T), start: usize, end: usize) ArrayList(T) {}
