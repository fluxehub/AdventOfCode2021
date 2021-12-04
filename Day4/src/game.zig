const std = @import("std");
const print = std.debug.print;

const Cell = struct {
    value: i32,
    marked: bool,

    pub fn mark(self: *Cell) void {
        self.marked = true;
    }
};

const Winner = struct {
    number: i32,
    board: *Board
};

pub const Board = struct {
    // Board is guaranteed to be 25 cells
    board: [25]Cell,

    pub fn setCell(self: *Board, x: usize, y: usize, value: i32) void {
        self.board[y * 5 + x].value = value;
    }

    pub fn mark(self: *Board, value: i32) void {
        for (self.board) |cell, i| {
            if (cell.value == value) {
                self.board[i].mark();
                return;
            }
        }
    }

    pub fn getCell(self: *Board, x: usize, y: usize) *Cell {
        return &self.board[y * 5 + x];
    }

    pub fn display(self: *Board) void {
        var y: usize = 0;
        while (y < 5) : (y += 1) {
            var x: usize = 0;
            while (x < 5): (x += 1) {
                const cell = self.getCell(x, y);

                if (cell.marked) {
                    print("\x1b[31m", .{});
                }

                if (cell.value < 10) {
                    print(" {d} ", .{cell.value});
                } else {
                    print("{d} ", .{cell.value});
                }

                print("\x1b[0m", .{});
            }
            print("\n", .{});
        }
    }

    pub fn fromReader(allocator: *std.mem.Allocator, reader: std.fs.File.Reader) !*Board {
        var board = try allocator.create(Board);

        var y: usize = 0;
        while (y < 5) : (y += 1) {
            // Lines are always 14 characters long
            var buf: [15]u8 = undefined;
            var line = try reader.readUntilDelimiter(&buf, '\n');
            
            var x: usize = 0;
            while (x < 5) : (x += 1) {
                const offset = if (x == 0) 0 else x * 3;
                const value = if (line[offset] == ' ') line[offset + 1..offset + 2] else line[offset..offset + 2];

                var number = try std.fmt.parseInt(i32, value, 10);
                board.setCell(x, y, number);
            }
        }

        return board;
    }

    pub fn checkWin(self: *Board) bool {
        // Check rows
        var y: usize = 0;
        while (y < 5) : (y += 1) {
            // This was originally a loop but for some reason, that only God or Hades may know, it didn't work. I assume it was out of some form of torment.
            if (self.getCell(0, y).marked and self.getCell(1, y).marked and self.getCell(2, y).marked and self.getCell(3, y).marked and self.getCell(4, y).marked) {
                return true;
            }
        }

        // Check columns
        var x: usize = 0;
        while (x < 5) : (x += 1) {
            if (self.getCell(x, 0).marked and self.getCell(x, 1).marked and self.getCell(x, 2).marked and self.getCell(x, 3).marked and self.getCell(x, 4).marked) {
                return true;
            }
        }

        return false;
    }

    pub fn score(self: *Board, number: i32) i32 {
        var sum: i32 = 0;

        for (self.board) |cell| {
            // This language is apparently broken somehow because doing if (!cell.marked) or if (cell.marked == false) straight up didn't work
            if (cell.marked) { 
                continue;
            }

            sum += cell.value;
        }

        return sum * number;
    }
};

pub fn run(boards: []*Board, numbers: []const i32) Winner {
    for (numbers) |number| {
        for (boards) |board| {
            board.mark(number);
            if (board.checkWin()) {
                return Winner { .number = number, .board = board, };
            }
        }
    }

    unreachable;
}