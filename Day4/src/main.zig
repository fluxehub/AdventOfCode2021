const std = @import("std");
const Game = @import("game.zig");
const Board = Game.Board;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const split = std.mem.split;
const print = std.debug.print;

fn run(allocator: *Allocator, filename: []const u8) !i32 {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const reader = file.reader();

    // Longest line is 291, round up to 300 anyway
    var buf = [_]u8{0} ** 300;

    // Read numbers in
    const firstLine = try reader.readUntilDelimiter(&buf, '\n');

    // Convert the line to an iterator
    var rawNumbers = std.mem.split(u8, firstLine, ",");

    var numbers = std.ArrayList(i32).init(allocator);
    defer numbers.deinit();

    // It's kinda silly to discard the iterator just to build another one,
    // but I wanted the game methods to receive a list of numbers, not strings
    while (rawNumbers.next()) |next| {

        // I tried my hardest to make this less hacky but I gave up
        const number = std.fmt.parseInt(i32, next, 10) catch { break; };
        try numbers.append(number);
    }

    var boards = std.ArrayList(*Board).init(allocator);
    defer boards.deinit();

    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |_| {
        var board = try Board.fromReader(allocator, reader);
        try boards.append(board);
    }
    
    const winner = Game.run(boards.items, numbers.items);
    const score = winner.board.score(winner.number);

    // Free boards
    for (boards.items) |board| {
        allocator.destroy(board);
    }

    return score;
}

pub fn main() anyerror!void {
    const filename = "input.txt";
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    print("{d}", .{try run(&arena.allocator, filename)});
}

test "Returns the correct winning score in test file" {
    const filename = "test.txt";
    var test_allocator = std.testing.allocator;
    
    const result = try run(test_allocator, filename);
    try std.testing.expect(result == 4512);
}