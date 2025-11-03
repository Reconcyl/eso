const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ascii = std.ascii;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var args = std.process.args();
    defer args.deinit();

    const exe_name = args.next() orelse @panic("can't get exe name");

    const in_file = args.next() orelse usage(exe_name);
    const out_file = args.next() orelse usage(exe_name);

    if (args.next()) |_| usage(exe_name);

    const code = blk: {
        const file = try std.fs.cwd().openFile(in_file, .{});
        defer file.close();

        const max_size = 1024 * 1024;
        break :blk try file.readToEndAlloc(alloc, max_size);
    };
    defer alloc.free(code);

    parse(alloc, code) catch |e| switch (e) {
        error.NoParse => {
            defer if (parse_err_is_owned) {
                alloc.free(parse_err);
            };
            var buffer: [128]u8 = undefined;
            var stderr = std.fs.File.stderr().writer(&buffer);
            try stderr.interface.print("{s}:{}: {s}\n", .{ in_file, line_no + 1, parse_err });
            try stderr.interface.flush();
            std.process.exit(1);
        },
        error.OutOfMemory => return e,
    };

    const file = try std.fs.cwd().createFile(out_file, .{});
    try save(&file);
}

fn usage(exe_name: []const u8) noreturn {
    var buffer: [64]u8 = undefined;
    var stderr = std.fs.File.stderr().writer(&buffer);
    stderr.interface.print("Usage: {s} [infile] [outfile]\n", .{exe_name}) catch {};
    stderr.interface.flush() catch {};
    std.process.exit(1);
}

var image: [1 << 16]u16 = undefined;
var image_pos: u16 = 0;
var label_dict: LabelDict = undefined;

var line_no: u32 = 0;
var parse_err: []const u8 = undefined;
var parse_err_is_owned: bool = false;

fn parse(alloc: Allocator, code: []u8) !void {
    label_dict = try LabelDict.init(alloc);
    defer label_dict.deinit();
    @memset(&image, 0);
    var rest = code;
    while (nextLine(&rest)) |line| try parseLine(line);
    try label_dict.assertNoUndefined();
}

fn nextLine(code: *[]u8) ?[]u8 {
    const curCode = code.*;
    if (curCode.len == 0) return null;
    if (std.mem.indexOfScalar(u8, curCode, '\n')) |idx| {
        code.* = curCode[idx + 1 ..];
        return curCode[0..idx];
    } else {
        code.*.len = 0;
        return curCode;
    }
}

fn parseLine(line: []u8) !void {
    var state = ParseState{};
    var i: u32 = 0;
    while (i < line.len) {
        switch (try state.update(line[i], &line[i])) {
            .none => {},
            .advance => i += 1,
            .stop => break,
        }
    }
    try state.endLine();
    line_no += 1;
}

const ParseState = struct {
    const Mode = enum { none, number, minus, dollar, label, label_offset, str, str_esc };
    const Res = enum { none, advance, stop };

    mode: Mode = .none,
    // used by modes: `.label`, `.label_offset`
    // len == 0 when entering `.label_offset` is a sentinel for `$`
    cur_label: []const u8 = undefined,
    // used by modes: `.number`, `.label_offset`
    cur_num: u16 = undefined,
    // used by modes: `.number`, `.label_offset`
    cur_sign: u1 = undefined,

    const Self = ParseState;

    fn update(self: *Self, c: u8, c_ptr: *const u8) !Res {
        switch (self.mode) {
            .none => if (ascii.isWhitespace(c) or c == ',') {
                return .advance;
            } else if (c == ';') {
                return .stop;
            } else if (ascii.isDigit(c)) {
                self.cur_num = 0;
                self.cur_sign = 0;
                self.mode = .number;
                return .none;
            } else if (c == '-') {
                self.mode = .minus;
                return .advance;
            } else if (c == '$') {
                self.mode = .dollar;
                return .advance;
            } else if (c == '"') {
                self.mode = .str;
                return .advance;
            } else if (ascii.isAlphabetic(c)) {
                self.mode = .label;
                // this is the easiest way to do it unfortunately
                self.cur_label.ptr = @ptrCast(c_ptr);
                self.cur_label.len = 0;
                return .none;
            } else if (c == '?') {
                // indicates an unspecified value
                try emit(0);
                return .advance;
            } else {
                parse_err = "invalid start of token";
                return error.NoParse;
            },

            .number => if (ascii.isDigit(c)) {
                const digitValue = c - '0';
                self.cur_num *%= 10;
                switch (self.cur_sign) {
                    0 => self.cur_num +%= digitValue,
                    1 => self.cur_num -%= digitValue,
                }
                return .advance;
            } else {
                try emit(self.cur_num);
                self.mode = .none;
                return .none;
            },

            .minus => if (ascii.isDigit(c)) {
                self.cur_num = 0;
                self.cur_sign = 1;
                self.mode = .number;
                return .none;
            } else {
                parse_err = "digit expected after -";
                return error.NoParse;
            },

            .dollar => if (c == '+') {
                self.cur_label.len = 0;
                self.cur_num = 0;
                self.cur_sign = 0;
                self.mode = .label_offset;
                return .advance;
            } else if (c == '-') {
                self.cur_label.len = 0;
                self.cur_num = 0;
                self.cur_sign = 1;
                self.mode = .label_offset;
                return .advance;
            } else {
                try emitRelative(0);
                self.mode = .none;
                return .none;
            },

            .label => if (ascii.isAlphanumeric(c)) {
                self.cur_label.len += 1;
                return .advance;
            } else if (c == ':') {
                try label_dict.define(self.cur_label);
                self.mode = .none;
                return .advance;
            } else if (c == '+') {
                self.cur_num = 0;
                self.cur_sign = 0;
                self.mode = .label_offset;
                return .advance;
            } else if (c == '-') {
                self.cur_num = 0;
                self.cur_sign = 1;
                self.mode = .label_offset;
                return .advance;
            } else {
                try emit(try label_dict.refer(self.cur_label, 0));
                self.mode = .none;
                return .advance;
            },

            .label_offset => if (ascii.isDigit(c)) {
                const digitValue = c - '0';
                self.cur_num *%= 10;
                switch (self.cur_sign) {
                    0 => self.cur_num +%= digitValue,
                    1 => self.cur_num -%= digitValue,
                }
                return .advance;
            } else {
                if (self.cur_label.len == 0) try emitRelative(self.cur_num) else try emit(try label_dict.refer(self.cur_label, self.cur_num));
                self.mode = .none;
                return .none;
            },

            .str => {
                switch (c) {
                    '"' => self.mode = .none,
                    '\\' => self.mode = .str_esc,
                    else => try emit(c),
                }
                return .advance;
            },

            .str_esc => {
                switch (c) {
                    'n' => try emit('\n'),
                    't' => try emit('\t'),
                    'r' => try emit('\r'),
                    '\\' => try emit('\\'),
                    '\'' => try emit('"'),
                    else => {
                        const err = "invalid escape sequence";
                        const note = " (the escape sequence for quotes is \\')";
                        if (c == '"') parse_err = err ++ note else parse_err = err;
                        return error.NoParse;
                    },
                }
                self.mode = .str;
                return .advance;
            },
        }
    }

    fn endLine(self: *Self) !void {
        switch (self.mode) {
            .str, .str_esc => {
                parse_err = "unterminated string literal";
                return error.NoParse;
            },
            else => {},
        }
        // the pointer is only used when parsing labels so it's OK to
        // pass an undefined value
        _ = try self.update(' ', undefined);
    }
};

fn emit(num: u16) !void {
    image[image_pos] = num;
    image_pos = std.math.add(u16, image_pos, 1) catch |e| switch (e) {
        error.Overflow => {
            parse_err = "the image is too large";
            return error.NoParse;
        },
    };
}

fn emitRelative(delta: u16) !void {
    try emit(image_pos +% delta);
}

fn save(f: *const std.fs.File) !void {
    var buf: []u8 = std.mem.sliceAsBytes(&image);
    // don't write trailing zero bytes
    while (buf.len > 0 and buf[buf.len - 1] == 0) {
        buf.len -= 1;
    }
    try f.writeAll(buf);
}

const LabelDict = struct {
    // the keys are views of the source code, not owned
    const Entries = std.StringArrayHashMap(LabelData);

    entries: Entries,

    fn init(alloc: Allocator) !LabelDict {
        var entries = Entries.init(alloc);
        // insert opcode mnemonics as "labels" to their values
        const opcodes = [_]([]const u8){
            "hlt",
            "nop",
            "get",
            "lit",
            "not",
            "add",
            "mul",
            "max",
            "dmp",
            "sav",
            "chr",
            "num",
            "chi",
        };
        inline for (opcodes, 0..) |name, i| {
            try entries.putNoClobber(name, .{ .pos = i });
        }
        return LabelDict{ .entries = entries };
    }

    fn refer(self: *LabelDict, name: []const u8, delta: u16) !u16 {
        const ally = self.entries.allocator;
        var res = try self.entries.getOrPut(name);

        var fwd_refs: *ArrayList(LabelFwdRef) = undefined;
        if (res.found_existing) {
            switch (res.value_ptr.*) {
                .pos => |n| return n +% delta,
                .fwd_refs => |*refs| fwd_refs = refs,
            }
        } else {
            const refs = ArrayList(LabelFwdRef).empty;
            res.value_ptr.* = .{ .fwd_refs = refs };
            fwd_refs = &res.value_ptr.fwd_refs;
        }

        try fwd_refs.append(ally, .{
            .pos = image_pos,
            .delta = delta,
        });
        return 0;
    }

    fn define(self: *LabelDict, name: []const u8) !void {
        const ally = self.entries.allocator;
        const res = try self.entries.getOrPut(name);
        if (res.found_existing) {
            switch (res.value_ptr.*) {
                .pos => {
                    parse_err = try std.fmt.allocPrint(
                        self.entries.allocator,
                        "label `{s}` defined twice",
                        .{res.key_ptr.*},
                    );
                    parse_err_is_owned = true;
                    return error.NoParse;
                },
                .fwd_refs => |*refs| {
                    for (refs.items) |fwd_ref| {
                        image[fwd_ref.pos] = image_pos +% fwd_ref.delta;
                    }
                    refs.deinit(ally);
                },
            }
        }
        res.value_ptr.* = .{ .pos = image_pos };
    }

    fn assertNoUndefined(self: *LabelDict) !void {
        var iter = self.entries.iterator();
        while (iter.next()) |entry| switch (entry.value_ptr.*) {
            .fwd_refs => {
                parse_err = try std.fmt.allocPrint(
                    self.entries.allocator,
                    "label `{s}` referenced but not defined",
                    .{entry.key_ptr.*},
                );
                parse_err_is_owned = true;
                return error.NoParse;
            },
            .pos => {},
        };
    }

    fn deinit(self: *LabelDict) void {
        const ally = self.entries.allocator;
        var iter = self.entries.iterator();
        while (iter.next()) |entry| switch (entry.value_ptr.*) {
            .fwd_refs => |*refs| refs.deinit(ally),
            .pos => {},
        };
        self.entries.deinit();
    }
};

const LabelData = union(enum) {
    pos: u16,
    fwd_refs: ArrayList(LabelFwdRef),
};

const LabelFwdRef = struct {
    pos: u16,
    delta: u16,
};
