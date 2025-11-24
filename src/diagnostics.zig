const std = @import("std");
const Token = @import("lexer.zig").Token;

pub const Diagnostic = struct {
    message: []const u8,
    loc: Token.Loc,
    severity: Severity,
    hint: ?[]const u8 = null,

    pub const Severity = enum {
        err,
        warning,
    };
};

pub const Diagnostics = struct {
    allocator: std.mem.Allocator,
    errors: std.ArrayListUnmanaged(Diagnostic),

    pub fn init(allocator: std.mem.Allocator) Diagnostics {
        return .{
            .allocator = allocator,
            .errors = .{},
        };
    }

    pub fn deinit(self: *Diagnostics) void {
        // We own the messages if they were allocated
        for (self.errors.items) |diag| {
            self.allocator.free(diag.message);
            if (diag.hint) |h| self.allocator.free(h);
        }
        self.errors.deinit(self.allocator);
    }

    pub fn addError(self: *Diagnostics, loc: Token.Loc, comptime fmt: []const u8, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.errors.append(self.allocator, .{
            .message = msg,
            .loc = loc,
            .severity = .err,
        });
    }

    pub fn addWarning(self: *Diagnostics, loc: Token.Loc, comptime fmt: []const u8, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.errors.append(self.allocator, .{
            .message = msg,
            .loc = loc,
            .severity = .warning,
        });
    }

    pub fn addErrorWithHint(self: *Diagnostics, loc: Token.Loc, hint: []const u8, comptime fmt: []const u8, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        const hint_msg = try self.allocator.dupe(u8, hint);
        try self.errors.append(self.allocator, .{
            .message = msg,
            .loc = loc,
            .severity = .err,
            .hint = hint_msg,
        });
    }

    pub fn hasErrors(self: *Diagnostics) bool {
        for (self.errors.items) |diag| {
            if (diag.severity == .err) return true;
        }
        return false;
    }

    pub fn report(self: *Diagnostics, source: []const u8) void {
        for (self.errors.items) |diag| {
            // Color codes
            const red = "\x1b[31m";
            const yellow = "\x1b[33m";
            const reset = "\x1b[0m";
            const bold = "\x1b[1m";
            const cyan = "\x1b[36m";

            const color = if (diag.severity == .err) red else yellow;
            const label = if (diag.severity == .err) "error" else "warning";

            // Print header: error: message
            std.debug.print("{s}{s}{s}: {s}{s}\n", .{ bold, color, label, reset, diag.message });

            // Print location: --> line:col
            std.debug.print("  {s}-->{s} {d}:{d}\n", .{ cyan, reset, diag.loc.line, diag.loc.col });

            // Find line start and end
            var line_start: usize = 0;
            var current_line: usize = 1;
            var i: usize = 0;
            while (i < source.len) {
                if (current_line == diag.loc.line) {
                    line_start = i;
                    break;
                }
                if (source[i] == '\n') {
                    current_line += 1;
                    line_start = i + 1;
                }
                i += 1;
            }

            var line_end = line_start;
            while (line_end < source.len and source[line_end] != '\n') : (line_end += 1) {}

            const line_content = source[line_start..line_end];

            // Print line number and content
            //   10 |     let x = conut + 1;
            std.debug.print("   {s}|\n{d: >4} |{s} {s}\n", .{ cyan, diag.loc.line, reset, line_content });

            // Print underline
            //   |             ^^^^^
            std.debug.print("   {s}|{s} ", .{ cyan, reset });

            // Calculate padding
            // diag.loc.col is 1-based index in the line
            var j: usize = 1;
            while (j < diag.loc.col) : (j += 1) {
                std.debug.print(" ", .{});
            }

            // Calculate length
            const len = if (diag.loc.end > diag.loc.start) diag.loc.end - diag.loc.start else 1;

            std.debug.print("{s}", .{color});
            var k: usize = 0;
            while (k < len) : (k += 1) {
                std.debug.print("^", .{});
            }
            std.debug.print("{s}\n", .{reset});

            if (diag.hint) |hint| {
                std.debug.print("   {s}={s} help: {s}\n", .{ cyan, reset, hint });
            }

            std.debug.print("\n", .{});
        }
    }
};
