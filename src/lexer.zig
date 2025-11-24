const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
        line: usize,
        col: usize,
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
        keyword_break,
        keyword_continue,
        keyword_struct,
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
        l_bracket,
        r_bracket,
        semicolon,
        comma,
        dot,
        colon,
        arrow,
        string_literal,
        float_literal,
    };
};

pub const Lexer = struct {
    source: []const u8,
    index: usize,
    line: usize,
    col: usize,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .index = 0,
            .line = 1,
            .col = 1,
        };
    }

    pub fn next(self: *Lexer) Token {
        self.skipWhitespace();

        const start = self.index;
        const start_line = self.line;
        const start_col = self.col;

        if (self.index >= self.source.len) {
            return .{ .tag = .eof, .loc = .{ .start = start, .end = start, .line = start_line, .col = start_col } };
        }

        const c = self.source[self.index];
        self.advanceChar();

        const tag: Token.Tag = switch (c) {
            '=' => if (self.match('=')) .equal_equal else .equal,
            '!' => if (self.match('=')) .bang_equal else .bang,
            '<' => if (self.match('=')) .less_equal else .less,
            '>' => if (self.match('=')) .greater_equal else .greater,
            '&' => if (self.match('&')) .ampersand_ampersand else .invalid,
            '|' => if (self.match('|')) .pipe_pipe else .invalid,
            '+' => .plus,
            '-' => if (self.match('>')) .arrow else .minus,
            '*' => .star,
            '/' => .slash,
            '(' => .l_paren,
            ')' => .r_paren,
            '{' => .l_brace,
            '}' => .r_brace,
            '[' => .l_bracket,
            ']' => .r_bracket,
            ';' => .semicolon,
            ',' => .comma,
            '.' => .dot,
            ':' => .colon,
            '"' => {
                // Opening quote already consumed by advanceChar
                while (self.index < self.source.len and self.source[self.index] != '"') {
                    if (self.source[self.index] == '\\' and self.index + 1 < self.source.len) {
                        self.advanceChar(); // backslash
                        self.advanceChar(); // escaped char
                    } else {
                        self.advanceChar();
                    }
                }
                if (self.index >= self.source.len) return .{ .tag = .invalid, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } }; // Unterminated string
                self.advanceChar(); // Closing quote
                return .{ .tag = .string_literal, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
            },
            '0'...'9' => {
                while (self.index < self.source.len) {
                    const digit = self.source[self.index];
                    if (!std.ascii.isDigit(digit)) break;
                    self.advanceChar();
                }
                // Check for fractional part
                if (self.index < self.source.len and self.source[self.index] == '.') {
                    // Lookahead to ensure it's a float and not a method call or property access
                    if (self.index + 1 < self.source.len and std.ascii.isDigit(self.source[self.index + 1])) {
                        self.advanceChar(); // Consume '.'
                        while (self.index < self.source.len) {
                            const digit = self.source[self.index];
                            if (!std.ascii.isDigit(digit)) break;
                            self.advanceChar();
                        }
                        return .{ .tag = .float_literal, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                    }
                }
                return .{ .tag = .number_literal, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
            },
            'a'...'z', 'A'...'Z', '_' => {
                while (self.index < self.source.len) {
                    const char = self.source[self.index];
                    if (!std.ascii.isAlphanumeric(char) and char != '_') break;
                    self.advanceChar();
                }
                const ident = self.source[start..self.index];
                if (std.mem.eql(u8, ident, "let")) return .{ .tag = .keyword_let, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "print")) return .{ .tag = .keyword_print, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "if")) return .{ .tag = .keyword_if, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "else")) return .{ .tag = .keyword_else, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "while")) return .{ .tag = .keyword_while, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "for")) return .{ .tag = .keyword_for, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "fn")) return .{ .tag = .keyword_fn, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "return")) return .{ .tag = .keyword_return, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "break")) return .{ .tag = .keyword_break, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "continue")) return .{ .tag = .keyword_continue, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "struct")) return .{ .tag = .keyword_struct, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "true")) return .{ .tag = .keyword_true, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                if (std.mem.eql(u8, ident, "false")) return .{ .tag = .keyword_false, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
                return .{ .tag = .identifier, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
            },
            else => .invalid,
        };

        return .{ .tag = tag, .loc = .{ .start = start, .end = self.index, .line = start_line, .col = start_col } };
    }

    fn advanceChar(self: *Lexer) void {
        if (self.index < self.source.len) {
            if (self.source[self.index] == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.index += 1;
        }
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.index >= self.source.len) return false;
        if (self.source[self.index] != expected) return false;
        self.advanceChar();
        return true;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.index < self.source.len) {
            const c = self.source[self.index];
            switch (c) {
                ' ', '\t', '\r' => self.advanceChar(),
                '\n' => self.advanceChar(),
                '/' => {
                    if (self.index + 1 < self.source.len and self.source[self.index + 1] == '/') {
                        // Comment: skip until newline
                        while (self.index < self.source.len and self.source[self.index] != '\n') {
                            self.advanceChar();
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

test "lexer arrays" {
    const source = "let a = [1, 2, 3]; a[0]";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_let, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_bracket, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.comma, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.comma, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_bracket, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_bracket, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_bracket, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer structs" {
    const source = "struct Point { x, y } let p = Point { x: 1, y: 2 }; p.x";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_struct, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.comma, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_let, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.colon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.comma, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.colon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.number_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.dot, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer arrow" {
    const source = "->";
    var lexer = Lexer.init(source);
    try std.testing.expectEqual(Token.Tag.arrow, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer break continue" {
    const source = "break; continue;";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_break, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_continue, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer floats" {
    const source = "let pi = 3.14; let x = 1.0; let y = 0.5;";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.keyword_let, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.float_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);

    try std.testing.expectEqual(Token.Tag.keyword_let, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.float_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);

    try std.testing.expectEqual(Token.Tag.keyword_let, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.float_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}
