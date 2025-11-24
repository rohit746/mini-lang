const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const Tag = enum {
        eof,
        invalid,
        identifier,
        number_literal,
        keyword_let,
        keyword_print,
        keyword_if,
        keyword_else,
        keyword_while,
        keyword_for,
        keyword_fn,
        keyword_return,
        keyword_true,
        keyword_false,
        bang,
        equal,
        equal_equal,
        bang_equal,
        less,
        less_equal,
        greater,
        greater_equal,
        ampersand_ampersand,
        pipe_pipe,
        plus,
        minus,
        star,
        slash,
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        semicolon,
        comma,
        string_literal,
    };
};

pub const Lexer = struct {
    source: []const u8,
    index: usize,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .index = 0,
        };
    }

    pub fn next(self: *Lexer) Token {
        self.skipWhitespace();

        const start = self.index;
        if (self.index >= self.source.len) {
            return .{ .tag = .eof, .loc = .{ .start = start, .end = start } };
        }

        const c = self.source[self.index];
        self.index += 1;

        const tag: Token.Tag = switch (c) {
            '=' => if (self.match('=')) .equal_equal else .equal,
            '!' => if (self.match('=')) .bang_equal else .bang,
            '<' => if (self.match('=')) .less_equal else .less,
            '>' => if (self.match('=')) .greater_equal else .greater,
            '&' => if (self.match('&')) .ampersand_ampersand else .invalid,
            '|' => if (self.match('|')) .pipe_pipe else .invalid,
            '+' => .plus,
            '-' => .minus,
            '*' => .star,
            '/' => .slash,
            '(' => .l_paren,
            ')' => .r_paren,
            '{' => .l_brace,
            '}' => .r_brace,
            ';' => .semicolon,
            ',' => .comma,
            '"' => {
                self.index += 1; // Skip opening quote
                while (self.index < self.source.len and self.source[self.index] != '"') {
                    if (self.source[self.index] == '\\' and self.index + 1 < self.source.len) {
                        self.index += 2; // Skip escaped char
                    } else {
                        self.index += 1;
                    }
                }
                if (self.index >= self.source.len) return .{ .tag = .invalid, .loc = .{ .start = start, .end = self.index } }; // Unterminated string
                self.index += 1; // Skip closing quote
                return .{ .tag = .string_literal, .loc = .{ .start = start, .end = self.index } };
            },
            '0'...'9' => {
                while (self.index < self.source.len) : (self.index += 1) {
                    const digit = self.source[self.index];
                    if (!std.ascii.isDigit(digit)) break;
                }
                return .{ .tag = .number_literal, .loc = .{ .start = start, .end = self.index } };
            },
            'a'...'z', 'A'...'Z', '_' => {
                while (self.index < self.source.len) : (self.index += 1) {
                    const char = self.source[self.index];
                    if (!std.ascii.isAlphanumeric(char) and char != '_') break;
                }
                const ident = self.source[start..self.index];
                if (std.mem.eql(u8, ident, "let")) return .{ .tag = .keyword_let, .loc = .{ .start = start, .end = self.index } };
                if (std.mem.eql(u8, ident, "print")) return .{ .tag = .keyword_print, .loc = .{ .start = start, .end = self.index } };
                if (std.mem.eql(u8, ident, "if")) return .{ .tag = .keyword_if, .loc = .{ .start = start, .end = self.index } };
                if (std.mem.eql(u8, ident, "else")) return .{ .tag = .keyword_else, .loc = .{ .start = start, .end = self.index } };
                if (std.mem.eql(u8, ident, "while")) return .{ .tag = .keyword_while, .loc = .{ .start = start, .end = self.index } };
                if (std.mem.eql(u8, ident, "for")) return .{ .tag = .keyword_for, .loc = .{ .start = start, .end = self.index } };
                if (std.mem.eql(u8, ident, "fn")) return .{ .tag = .keyword_fn, .loc = .{ .start = start, .end = self.index } };
                if (std.mem.eql(u8, ident, "return")) return .{ .tag = .keyword_return, .loc = .{ .start = start, .end = self.index } };
                if (std.mem.eql(u8, ident, "true")) return .{ .tag = .keyword_true, .loc = .{ .start = start, .end = self.index } };
                if (std.mem.eql(u8, ident, "false")) return .{ .tag = .keyword_false, .loc = .{ .start = start, .end = self.index } };
                return .{ .tag = .identifier, .loc = .{ .start = start, .end = self.index } };
            },
            else => .invalid,
        };

        return .{ .tag = tag, .loc = .{ .start = start, .end = self.index } };
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.index >= self.source.len) return false;
        if (self.source[self.index] != expected) return false;
        self.index += 1;
        return true;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.index < self.source.len) {
            const c = self.source[self.index];
            switch (c) {
                ' ', '\t', '\n', '\r' => self.index += 1,
                '/' => {
                    if (self.index + 1 < self.source.len and self.source[self.index + 1] == '/') {
                        // Comment: skip until newline
                        while (self.index < self.source.len and self.source[self.index] != '\n') {
                            self.index += 1;
                        }
                    } else {
                        return;
                    }
                },
                else => break,
            }
        }
    }
};

test "lexer basic" {
    const source = "let x = 10;";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_let, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer arithmetic" {
    const source = "print(x + 5 * 2);";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_print, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.plus, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.star, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
}

test "lexer control flow" {
    const source = "if (x == 10) { print(x); } else { while (y < 5) { y = y + 1; } }";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_if, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal_equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_print, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_else, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_while, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.less, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.plus, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer string" {
    const source = "let s = \"hello world\";";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_let, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.string_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
}

test "lexer booleans" {
    const source = "true && false || true";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_true, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.ampersand_ampersand, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_false, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.pipe_pipe, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_true, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer for loop" {
    const source = "for (let i = 0; i < 10; i = i + 1) { print(i); }";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_for, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_let, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.less, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.plus, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_print, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}
