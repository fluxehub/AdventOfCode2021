const std = @import("std");
const Game = @import("game.zig");
const Board = Game.Board;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const split = std.mem.split;
const print = std.debug.print;

const Data = struct {
    numbers: []i32,
    boards: []*Board,
};

fn load(allocator: *Allocator, filename: []const u8) !Data {
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

    // It's kinda silly to discard the iterator just to build another one,
    // but I wanted the game methods to receive a list of numbers, not strings
    while (rawNumbers.next()) |next| {

        // I tried my hardest to make this less hacky but I gave up
        const number = std.fmt.parseInt(i32, next, 10) catch { break; };
        try numbers.append(number);
    }

    var boards = std.ArrayList(*Board).init(allocator);

    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |_| {
        var board = try Board.fromReader(allocator, reader);
        try boards.append(board);
    }

    return Data { .numbers = numbers.toOwnedSlice(), .boards = boards.toOwnedSlice() };
}

fn part1(allocator: *Allocator, filename: []const u8) !i32 {
    var data = try load(allocator, filename);
    defer allocator.free(data.boards);
    defer allocator.free(data.numbers);
    
    const winner = Game.run(data.boards, data.numbers);
    const score = winner.board.score(winner.number);

    return score;
}

fn part2(allocator: *Allocator, filename: []const u8) !i32 {
    var data = try load(allocator, filename);
    defer allocator.free(data.boards);
    defer allocator.free(data.numbers);
    
    const winner = try Game.findLastBoard(allocator, data.boards, data.numbers);
    const score = winner.board.score(winner.number);

    return score;
}

pub fn main() anyerror!void {
    const filename = "input.txt";
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    
    print("Part 1: {d}\n", .{try part1(&arena.allocator, filename)});
    print("Part 2: {d}", .{try part2(&arena.allocator, filename)});
}

test "Returns the correct winning score in test file" {
    const filename = "test.txt";
    var test_allocator = std.testing.allocator;
    
    const result = try part1(test_allocator, filename);
    try std.testing.expect(result == 4512);
}

test "Returns the score for the last winning board in test file" {
    const filename = "test.txt";
    var test_allocator = std.testing.allocator;
    
    const result = try part2(test_allocator, filename);
    try std.testing.expect(result == 1924);
}