const std = @import("std");
const c = @cImport({
    @cInclude("stdlib.h");
    @cInclude("compiler.h");
});

fn perr(stderr: *const std.fs.File.Writer, comptime fmt: []const u8, args: anytype) void {
    stderr.print(fmt ++ "\n", args) catch {};
}

pub fn main() !u8 {
    // read input code
    const alloc = std.heap.c_allocator;
    const input_buf = try std.io.getStdIn().readToEndAlloc(alloc, 1024 * 1024);
    defer alloc.free(input_buf);

    // initialize Futhark context and buffers
    const stderr = std.io.getStdErr().writer();
    const cfg = c.futhark_context_config_new() orelse {
        perr(&stderr, "couldn't initialize Futhark config", .{});
        return 1;
    };
    // c.futhark_context_config_set_cache_file(cfg, "target/futhark_cache");
    defer c.futhark_context_config_free(cfg);
    const ctx = c.futhark_context_new(cfg) orelse {
        perr(&stderr, "couldn't allocate Futhark context", .{});
        return 1;
    };
    defer c.futhark_context_free(ctx);
    if (c.futhark_context_get_error(ctx)) |err| {
        defer c.free(err);
        perr(&stderr, "couldn't initialize Futhark context: {s}", .{err});
        return 1;
    }
    const fut_input = c.futhark_new_u8_1d(ctx, input_buf.ptr, @intCast(i64, input_buf.len)) orelse {
        const err = c.futhark_context_get_error(ctx);
        defer c.free(err);
        perr(&stderr, "couldn't allocate Futhark array: {s}", .{err});
        return 1;
    };
    defer if (c.futhark_free_u8_1d(ctx, fut_input) != 0) {
        const err = c.futhark_context_get_error(ctx);
        defer c.free(err);
        perr(&stderr, "couldn't free Futhark array: {s}", .{err});
    };

    // call Futhark main
    var fut_output_1: bool = undefined;
    var fut_output_2: ?*c.futhark_u8_1d = undefined;
    if (c.futhark_entry_main(ctx, &fut_output_1, &fut_output_2, fut_input) != 0) {
        const err = c.futhark_context_get_error(ctx);
        defer c.free(err);
        perr(&stderr, "Futhark error (main):\n{s}", .{err});
        return 1;
    }
    defer if (c.futhark_free_u8_1d(ctx, fut_output_2) != 0) {
        const err = c.futhark_context_get_error(ctx);
        defer c.free(err);
        perr(&stderr, "couldn't free Futhark array: {s}", .{err});
    };
    if (c.futhark_context_sync(ctx) != 0) {
        const err = c.futhark_context_get_error(ctx);
        defer c.free(err);
        perr(&stderr, "Futhark error (main sync):\n{s}", .{err});
        return 1;
    }

    // copy the result into a new buffer
    const output_len = std.math.cast(
        usize,
        c.futhark_shape_u8_1d(ctx, fut_output_2)[0],
    ) catch return error.OutputNegativeLen;
    const output_buf = try alloc.alloc(u8, output_len);
    defer alloc.free(output_buf);
    if (c.futhark_values_u8_1d(ctx, fut_output_2, output_buf.ptr) != 0) {
        const err = c.futhark_context_get_error(ctx);
        defer c.free(err);
        perr(&stderr, "Futhark error (copying output):\n{s}", .{err});
        return 1;
    }
    if (c.futhark_context_sync(ctx) != 0) {
        const err = c.futhark_context_get_error(ctx);
        defer c.free(err);
        perr(&stderr, "Futhark error (copying output sync):\n{s}", .{err});
        return 1;
    }

    if (!fut_output_1) {
        perr(&stderr, "Falafel error:\n{s}", .{output_buf});
        return 1;
    } else {
        try std.io.getStdOut().writeAll(output_buf);
        return 0;
    }
}
