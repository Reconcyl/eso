const std = @import("std");
const Allocator = std.mem.Allocator;

const Arr = std.ArrayListUnmanaged(u32);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const program = program: {
        const program_text = program_text: {
            const in_file = in_file: {
                var args = std.process.args();
                defer args.deinit();
                const exe_name = try args.next(alloc) orelse return;
                defer alloc.free(exe_name);

                const path = try args.next(alloc) orelse {
                    const stderr = std.io.getStdErr().writer();
                    stderr.print("Usage: {s} [infile]\n", .{exe_name}) catch {};
                    return;
                };
                defer alloc.free(path);

                break :in_file try std.fs.cwd().openFile(path, .{});
            };
            defer in_file.close();
            const max_size = 1024 * 1024;
            break :program_text try in_file.readToEndAlloc(alloc, max_size);
        };
        defer alloc.free(program_text);
        var program = Arr{};
        errdefer program.deinit(alloc);
        var iter = std.mem.split(u8, program_text, "\n");
        while (iter.next()) |line_raw| {
            var line = line_raw;
            if (line.len > 1 and line[line.len - 1] == '\r')
                line.len -= 1;
            const val = std.fmt.parseInt(u32, line, 10) catch 0;
            try program.append(alloc, val);
        }
        break :program program;
    };
    var state = State{ .memory = program };
    defer state.deinit(alloc);

    const result = try state.exec(alloc);
    const stdout = std.io.getStdOut().writer();
    stdout.print("{any}\n", .{result}) catch {};
}

const State = struct {
    memory: Arr,
    stack: Arr = .{},

    fn deinit(self: *State, alloc: Allocator) void {
        self.memory.deinit(alloc);
        self.stack.deinit(alloc);
    }

    fn get(self: State, idx: u32) u32 {
        return if (idx < self.memory.items.len) self.memory.items[idx] else 0;
    }

    fn set(self: *State, alloc: Allocator, idx: u32, val: u32) !void {
        if (idx == std.math.maxInt(usize)) @panic("too big...");
        const padding = @as(usize, idx) + 1 -| self.memory.items.len;
        try self.memory.appendNTimes(alloc, 0, padding);
        self.memory.items[idx] = val;
    }

    fn pop(self: *State) u32 {
        return self.stack.popOrNull() orelse 0;
    }

    fn peekPtr(self: State) ?*u32 {
        const stack = self.stack.items;
        return if (stack.len > 0) &stack[stack.len - 1] else null;
    }

    fn peek(self: State) u32 {
        return (self.peekPtr() orelse return 0).*;
    }

    fn push(self: *State, alloc: Allocator, x: u32) !void {
        try self.stack.append(alloc, x);
    }

    fn exec(self: *State, alloc: Allocator) ![]const u32 {
        var idx: u32 = 0;
        while (true) {
            const next_idx = self.get(idx);
            const ht = self.stack.items.len;
            switch (next_idx % 10) {
                // PushNxt
                0 => try self.push(alloc, self.get(next_idx +% 1)),
                // PushAddr
                1 => try self.push(alloc, self.get(self.pop())),
                // PopAddr
                2 => try self.set(alloc, self.pop(), self.pop()),
                // Duplicate
                3 => try self.push(alloc, self.peek()),
                // Swap
                4 => if (ht >= 2) {
                    const stack = self.stack.items;
                    std.mem.swap(
                        u32,
                        &stack[stack.len - 1],
                        &stack[stack.len - 2],
                    );
                } else try self.push(alloc, 0),
                // Add
                5 => if (ht >= 2) {
                    const x = self.pop();
                    self.peekPtr().?.* +%= x;
                },
                // Sub
                6 => if (ht >= 2) {
                    const x = self.pop();
                    self.peekPtr().?.* -%= x;
                } else if (ht == 1) {
                    self.peekPtr().?.* *%= std.math.maxInt(u32);
                },
                // Mul
                7 => if (ht >= 2) {
                    const x = self.pop();
                    self.peekPtr().?.* *%= x;
                },
                // Div
                8 => if (ht >= 2) {
                    const x = self.pop();
                    if (x == 0) @panic("division by zero");
                    self.peekPtr().?.* /= x;
                },
                // Quit
                9 => {
                    const next_next_idx = self.get(next_idx);
                    return self.memory.items[next_next_idx..];
                },
                else => unreachable,
            }
            idx = next_idx;
        }
    }
};
