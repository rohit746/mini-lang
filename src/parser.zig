const std = @import("std");
const Ast = @import("ast.zig");
const LexerModule = @import("lexer.zig");
const Token = LexerModule.Token;
const Lexer = LexerModule.Lexer;

pub const ParseError = error{
    UnexpectedToken,
    ExpectedStatement,
    ExpectedIdentifier,
    OutOfMemory,
    InvalidCharacter,
    Overflow,
};

pub const Parser = struct {
    lexer: Lexer,
    allocator: std.mem.Allocator,
    current_token: Token,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        var lexer = Lexer.init(source);
        const first = lexer.next();
        return .{
            .lexer = lexer,
            .allocator = allocator,
            .current_token = first,
        };
    }

    fn advance(self: *Parser) void {
        self.current_token = self.lexer.next();
    }

    fn eat(self: *Parser, tag: Token.Tag) ParseError!void {
        if (self.current_token.tag == tag) {
            self.advance();
        } else {
            return error.UnexpectedToken;
        }
    }

    pub fn parse(self: *Parser) ParseError!Ast.Program {
        var statements = std.ArrayListUnmanaged(Ast.Stmt){};
        errdefer statements.deinit(self.allocator);

        while (self.current_token.tag != .eof) {
            const stmt = try self.parseStatement();
            try statements.append(self.allocator, stmt);
        }

        return Ast.Program{ .statements = try statements.toOwnedSlice(self.allocator) };
    }

    fn parseStatement(self: *Parser) ParseError!Ast.Stmt {
        if (self.current_token.tag == .keyword_let) {
            return self.parseVarDecl();
        } else if (self.current_token.tag == .keyword_print) {
            return self.parsePrintStmt();
        } else if (self.current_token.tag == .keyword_if) {
            return self.parseIfStmt();
        } else if (self.current_token.tag == .keyword_while) {
            return self.parseWhileStmt();
        } else if (self.current_token.tag == .keyword_for) {
            return self.parseForStmt();
        } else if (self.current_token.tag == .keyword_fn) {
            return self.parseFnDecl();
        } else if (self.current_token.tag == .keyword_struct) {
            return self.parseStructDecl();
        } else if (self.current_token.tag == .keyword_return) {
            return self.parseReturnStmt();
        } else if (self.current_token.tag == .l_brace) {
            return self.parseBlock();
        } else if (self.current_token.tag == .identifier) {
            const start_loc = self.current_token.loc;
            const name = self.lexer.source[start_loc.start..start_loc.end];
            self.advance(); // Eat identifier

            if (self.current_token.tag == .equal) {
                self.advance(); // Eat `=`
                const value = try self.parseExpression();
                try self.eat(.semicolon);
                return Ast.Stmt{ .assign = .{ .name = name, .value = value } };
            } else if (self.current_token.tag == .l_bracket) {
                self.advance(); // Eat `[`
                const index = try self.parseExpression();
                try self.eat(.r_bracket);
                try self.eat(.equal);
                const value = try self.parseExpression();
                try self.eat(.semicolon);
                return Ast.Stmt{ .array_assign = .{ .name = name, .index = index, .value = value } };
            } else if (self.current_token.tag == .l_paren) {
                try self.eat(.l_paren);
                var args = std.ArrayListUnmanaged(*Ast.Expr){};
                errdefer args.deinit(self.allocator);

                if (self.current_token.tag != .r_paren) {
                    while (true) {
                        const arg = try self.parseExpression();
                        try args.append(self.allocator, arg);
                        if (self.current_token.tag == .comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                try self.eat(.r_paren);
                try self.eat(.semicolon);
                const call_expr = try self.allocator.create(Ast.Expr);
                call_expr.* = Ast.Expr{ .call = .{ .callee = name, .args = try args.toOwnedSlice(self.allocator) } };

                return error.ExpectedStatement;
            } else {
                return error.ExpectedStatement;
            }
        } else {
            return error.ExpectedStatement;
        }
    }

    fn parseFnDecl(self: *Parser) ParseError!Ast.Stmt {
        try self.eat(.keyword_fn);

        if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
        const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
        self.advance();

        try self.eat(.l_paren);
        var params = std.ArrayListUnmanaged([]const u8){};
        errdefer params.deinit(self.allocator);

        if (self.current_token.tag != .r_paren) {
            while (true) {
                if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
                const param_name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
                try params.append(self.allocator, param_name);
                self.advance();

                if (self.current_token.tag == .comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        try self.eat(.r_paren);

        const body = try self.allocator.create(Ast.Stmt);
        body.* = try self.parseBlock();

        return Ast.Stmt{ .fn_decl = .{ .name = name, .params = try params.toOwnedSlice(self.allocator), .body = body } };
    }

    fn parseReturnStmt(self: *Parser) ParseError!Ast.Stmt {
        try self.eat(.keyword_return);
        var value: ?*Ast.Expr = null;
        if (self.current_token.tag != .semicolon) {
            value = try self.parseExpression();
        }
        try self.eat(.semicolon);
        return Ast.Stmt{ .return_stmt = value };
    }

    fn parseBlock(self: *Parser) ParseError!Ast.Stmt {
        try self.eat(.l_brace);
        var statements = std.ArrayListUnmanaged(Ast.Stmt){};
        errdefer statements.deinit(self.allocator);

        while (self.current_token.tag != .r_brace and self.current_token.tag != .eof) {
            const stmt = try self.parseStatement();
            try statements.append(self.allocator, stmt);
        }
        try self.eat(.r_brace);

        return Ast.Stmt{ .block = try statements.toOwnedSlice(self.allocator) };
    }

    fn parseIfStmt(self: *Parser) ParseError!Ast.Stmt {
        try self.eat(.keyword_if);
        try self.eat(.l_paren);
        const condition = try self.parseExpression();
        try self.eat(.r_paren);

        const then_branch = try self.allocator.create(Ast.Stmt);
        then_branch.* = try self.parseStatement();

        var else_branch: ?*Ast.Stmt = null;
        if (self.current_token.tag == .keyword_else) {
            self.advance();
            else_branch = try self.allocator.create(Ast.Stmt);
            else_branch.?.* = try self.parseStatement();
        }

        return Ast.Stmt{ .if_stmt = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch } };
    }

    fn parseWhileStmt(self: *Parser) ParseError!Ast.Stmt {
        try self.eat(.keyword_while);
        try self.eat(.l_paren);
        const condition = try self.parseExpression();
        try self.eat(.r_paren);

        const body = try self.allocator.create(Ast.Stmt);
        body.* = try self.parseStatement();

        return Ast.Stmt{ .while_stmt = .{ .condition = condition, .body = body } };
    }

    fn parseForStmt(self: *Parser) ParseError!Ast.Stmt {
        try self.eat(.keyword_for);
        try self.eat(.l_paren);

        var init_stmt: ?*Ast.Stmt = null;
        if (self.current_token.tag != .semicolon) {
            const stmt = try self.parseStatement();
            init_stmt = try self.allocator.create(Ast.Stmt);
            init_stmt.?.* = stmt;
        } else {
            try self.eat(.semicolon);
        }

        var condition: ?*Ast.Expr = null;
        if (self.current_token.tag != .semicolon) {
            condition = try self.parseExpression();
        }
        try self.eat(.semicolon);

        var increment: ?*Ast.Stmt = null;
        if (self.current_token.tag != .r_paren) {
            if (self.current_token.tag == .identifier) {
                const start_loc = self.current_token.loc;
                const name = self.lexer.source[start_loc.start..start_loc.end];
                self.advance();

                if (self.current_token.tag == .equal) {
                    self.advance();
                    const value = try self.parseExpression();
                    increment = try self.allocator.create(Ast.Stmt);
                    increment.?.* = Ast.Stmt{ .assign = .{ .name = name, .value = value } };
                } else {
                    return error.ExpectedStatement;
                }
            } else {
                return error.ExpectedStatement;
            }
        }
        try self.eat(.r_paren);

        const body = try self.allocator.create(Ast.Stmt);
        body.* = try self.parseStatement();

        return Ast.Stmt{ .for_stmt = .{ .init = init_stmt, .condition = condition, .increment = increment, .body = body } };
    }

    fn parseAssignment(self: *Parser) ParseError!Ast.Stmt {
        const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
        try self.eat(.identifier);
        try self.eat(.equal);
        const value = try self.parseExpression();
        try self.eat(.semicolon);
        return Ast.Stmt{ .assign = .{ .name = name, .value = value } };
    }

    fn parseVarDecl(self: *Parser) ParseError!Ast.Stmt {
        try self.eat(.keyword_let);

        if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
        const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
        self.advance();

        try self.eat(.equal);
        const value = try self.parseExpression();
        try self.eat(.semicolon);

        return Ast.Stmt{ .let = .{ .name = name, .value = value } };
    }

    fn parsePrintStmt(self: *Parser) ParseError!Ast.Stmt {
        try self.eat(.keyword_print);
        try self.eat(.l_paren);
        const expr = try self.parseExpression();
        try self.eat(.r_paren);
        try self.eat(.semicolon);
        return Ast.Stmt{ .print = expr };
    }

    fn parseExpression(self: *Parser) ParseError!*Ast.Expr {
        return self.parseLogicOr();
    }

    fn parseLogicOr(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseLogicAnd();

        while (self.current_token.tag == .pipe_pipe) {
            self.advance();
            const right = try self.parseLogicAnd();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .binary = .{ .left = left, .op = .logic_or, .right = right } };
            left = new_node;
        }

        return left;
    }

    fn parseLogicAnd(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseEquality();

        while (self.current_token.tag == .ampersand_ampersand) {
            self.advance();
            const right = try self.parseEquality();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .binary = .{ .left = left, .op = .logic_and, .right = right } };
            left = new_node;
        }

        return left;
    }

    fn parseEquality(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseComparison();

        while (self.current_token.tag == .equal_equal or self.current_token.tag == .bang_equal) {
            const op: Ast.BinaryOp = if (self.current_token.tag == .equal_equal) .equal_equal else .bang_equal;
            self.advance();
            const right = try self.parseComparison();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .binary = .{ .left = left, .op = op, .right = right } };
            left = new_node;
        }

        return left;
    }

    fn parseComparison(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseTerm();

        while (self.current_token.tag == .less or self.current_token.tag == .less_equal or
            self.current_token.tag == .greater or self.current_token.tag == .greater_equal)
        {
            const op: Ast.BinaryOp = switch (self.current_token.tag) {
                .less => .less,
                .less_equal => .less_equal,
                .greater => .greater,
                .greater_equal => .greater_equal,
                else => unreachable,
            };
            self.advance();
            const right = try self.parseTerm();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .binary = .{ .left = left, .op = op, .right = right } };
            left = new_node;
        }

        return left;
    }

    fn parseTerm(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseFactor();

        while (self.current_token.tag == .plus or self.current_token.tag == .minus) {
            const op: Ast.BinaryOp = if (self.current_token.tag == .plus) .add else .sub;
            self.advance();
            const right = try self.parseFactor();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .binary = .{ .left = left, .op = op, .right = right } };
            left = new_node;
        }

        return left;
    }

    fn parseFactor(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseUnary();

        while (self.current_token.tag == .star or self.current_token.tag == .slash) {
            const op: Ast.BinaryOp = if (self.current_token.tag == .star) .mul else .div;
            self.advance();
            const right = try self.parseUnary();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .binary = .{ .left = left, .op = op, .right = right } };
            left = new_node;
        }

        return left;
    }

    fn parseUnary(self: *Parser) ParseError!*Ast.Expr {
        if (self.current_token.tag == .bang or self.current_token.tag == .minus) {
            const op: Ast.UnaryOp = if (self.current_token.tag == .bang) .bang else .minus;
            self.advance();
            const right = try self.parseUnary();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .unary = .{ .op = op, .right = right } };
            return new_node;
        }
        return self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!*Ast.Expr {
        var expr: *Ast.Expr = undefined;

        if (self.current_token.tag == .number_literal) {
            const text = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
            const num = try std.fmt.parseInt(i64, text, 10);
            self.advance();
            const node = try self.allocator.create(Ast.Expr);
            node.* = Ast.Expr{ .number = num };
            expr = node;
        } else if (self.current_token.tag == .string_literal) {
            // Strip quotes
            const raw = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
            const content = raw[1 .. raw.len - 1];
            self.advance();
            const node = try self.allocator.create(Ast.Expr);
            node.* = Ast.Expr{ .string = content };
            expr = node;
        } else if (self.current_token.tag == .keyword_true) {
            self.advance();
            const node = try self.allocator.create(Ast.Expr);
            node.* = Ast.Expr{ .boolean = true };
            expr = node;
        } else if (self.current_token.tag == .keyword_false) {
            self.advance();
            const node = try self.allocator.create(Ast.Expr);
            node.* = Ast.Expr{ .boolean = false };
            expr = node;
        } else if (self.current_token.tag == .identifier) {
            const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
            self.advance();

            if (self.current_token.tag == .l_paren) {
                // Function call
                self.advance(); // eat (
                var args = std.ArrayListUnmanaged(*Ast.Expr){};
                errdefer args.deinit(self.allocator);

                if (self.current_token.tag != .r_paren) {
                    while (true) {
                        const arg = try self.parseExpression();
                        try args.append(self.allocator, arg);
                        if (self.current_token.tag == .comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                try self.eat(.r_paren);

                const node = try self.allocator.create(Ast.Expr);
                node.* = Ast.Expr{ .call = .{ .callee = name, .args = try args.toOwnedSlice(self.allocator) } };
                expr = node;
            } else if (self.current_token.tag == .l_brace) {
                // Struct Literal
                self.advance(); // eat {
                var fields = std.ArrayListUnmanaged(Ast.StructFieldInit){};
                errdefer fields.deinit(self.allocator);

                if (self.current_token.tag != .r_brace) {
                    while (true) {
                        if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
                        const field_name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
                        self.advance();

                        try self.eat(.colon);
                        const value = try self.parseExpression();
                        try fields.append(self.allocator, .{ .name = field_name, .value = value });

                        if (self.current_token.tag == .comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                try self.eat(.r_brace);

                const node = try self.allocator.create(Ast.Expr);
                node.* = Ast.Expr{ .struct_literal = .{ .struct_name = name, .fields = try fields.toOwnedSlice(self.allocator) } };
                expr = node;
            } else {
                const node = try self.allocator.create(Ast.Expr);
                node.* = Ast.Expr{ .identifier = name };
                expr = node;
            }
        } else if (self.current_token.tag == .l_paren) {
            self.advance();
            expr = try self.parseExpression();
            try self.eat(.r_paren);
        } else if (self.current_token.tag == .l_bracket) {
            self.advance(); // eat [
            var elements = std.ArrayListUnmanaged(*Ast.Expr){};
            errdefer elements.deinit(self.allocator);

            if (self.current_token.tag != .r_bracket) {
                while (true) {
                    const elem = try self.parseExpression();
                    try elements.append(self.allocator, elem);
                    if (self.current_token.tag == .comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            try self.eat(.r_bracket);
            const node = try self.allocator.create(Ast.Expr);
            node.* = Ast.Expr{ .array_literal = try elements.toOwnedSlice(self.allocator) };
            expr = node;
        } else {
            return error.UnexpectedToken;
        }

        // Postfix operators (indexing, field access)
        while (true) {
            if (self.current_token.tag == .l_bracket) {
                self.advance(); // eat [
                const index_expr = try self.parseExpression();
                try self.eat(.r_bracket);
                const node = try self.allocator.create(Ast.Expr);
                node.* = Ast.Expr{ .index = .{ .callee = expr, .index = index_expr } };
                expr = node;
            } else if (self.current_token.tag == .dot) {
                self.advance(); // eat .
                if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
                const field_name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
                self.advance();
                const node = try self.allocator.create(Ast.Expr);
                node.* = Ast.Expr{ .field_access = .{ .object = expr, .field = field_name } };
                expr = node;
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseStructDecl(self: *Parser) ParseError!Ast.Stmt {
        try self.eat(.keyword_struct);

        if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
        const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
        self.advance();

        try self.eat(.l_brace);
        var fields = std.ArrayListUnmanaged([]const u8){};
        errdefer fields.deinit(self.allocator);

        if (self.current_token.tag != .r_brace) {
            while (true) {
                if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
                const field_name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
                try fields.append(self.allocator, field_name);
                self.advance();

                if (self.current_token.tag == .comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        try self.eat(.r_brace);

        return Ast.Stmt{ .struct_decl = .{ .name = name, .fields = try fields.toOwnedSlice(self.allocator) } };
    }
};

test "parser basic" {
    const source =
        \\let x = 10;
        \\print(x + 5);
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 2), program.statements.len);
}

test "parser control flow" {
    const source =
        \\if (x < 10) {
        \\    print(x);
        \\} else {
        \\    x = x + 1;
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
    const stmt = program.statements[0];
    try std.testing.expect(stmt == .if_stmt);
}

test "parser string" {
    const source = "let s = \"hello\";";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
    const stmt = program.statements[0];
    switch (stmt) {
        .let => |decl| {
            try std.testing.expectEqualStrings("s", decl.name);
            switch (decl.value.*) {
                .string => |s| try std.testing.expectEqualStrings("hello", s),
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }
}

test "parser boolean" {
    const source = "let b = true && false || true;";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
    const stmt = program.statements[0];
    switch (stmt) {
        .let => |decl| {
            try std.testing.expectEqualStrings("b", decl.name);
            // Check structure: (true && false) || true
            switch (decl.value.*) {
                .binary => |bin| {
                    try std.testing.expectEqual(Ast.BinaryOp.logic_or, bin.op);
                    switch (bin.left.*) {
                        .binary => |left_bin| {
                            try std.testing.expectEqual(Ast.BinaryOp.logic_and, left_bin.op);
                        },
                        else => return error.TestUnexpectedResult,
                    }
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }
}

test "parser for loop" {
    const source =
        \\for (let i = 0; i < 10; i = i + 1) {
        \\    print(i);
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
    const stmt = program.statements[0];
    switch (stmt) {
        .for_stmt => |f| {
            // Check init
            if (f.init) |init_stmt| {
                switch (init_stmt.*) {
                    .let => |l| {
                        try std.testing.expectEqualStrings("i", l.name);
                    },
                    else => return error.TestUnexpectedResult,
                }
            } else return error.TestUnexpectedResult;

            // Check condition
            if (f.condition) |cond| {
                switch (cond.*) {
                    .binary => |bin| {
                        try std.testing.expectEqual(Ast.BinaryOp.less, bin.op);
                    },
                    else => return error.TestUnexpectedResult,
                }
            } else return error.TestUnexpectedResult;

            // Check increment
            if (f.increment) |incr| {
                switch (incr.*) {
                    .assign => |a| {
                        try std.testing.expectEqualStrings("i", a.name);
                    },
                    else => return error.TestUnexpectedResult,
                }
            } else return error.TestUnexpectedResult;
        },
        else => return error.TestUnexpectedResult,
    }
}

test "parser arrays" {
    const source =
        \\let a = [1, 2, 3];
        \\let b = a[0];
        \\a[1] = 10;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 3), program.statements.len);

    // let a = [1, 2, 3];
    const stmt1 = program.statements[0];
    switch (stmt1) {
        .let => |l| {
            try std.testing.expectEqualStrings("a", l.name);
            switch (l.value.*) {
                .array_literal => |arr| {
                    try std.testing.expectEqual(@as(usize, 3), arr.len);
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }

    // let b = a[0];
    const stmt2 = program.statements[1];
    switch (stmt2) {
        .let => |l| {
            try std.testing.expectEqualStrings("b", l.name);
            switch (l.value.*) {
                .index => |idx| {
                    switch (idx.callee.*) {
                        .identifier => |id| try std.testing.expectEqualStrings("a", id),
                        else => return error.TestUnexpectedResult,
                    }
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }

    // a[1] = 10;
    const stmt3 = program.statements[2];
    switch (stmt3) {
        .array_assign => |aa| {
            try std.testing.expectEqualStrings("a", aa.name);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "parser structs" {
    const source =
        \\struct Point { x, y }
        \\let p = Point { x: 1, y: 2 };
        \\print(p.x);
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 3), program.statements.len);

    // struct Point { x, y }
    const stmt1 = program.statements[0];
    switch (stmt1) {
        .struct_decl => |s| {
            try std.testing.expectEqualStrings("Point", s.name);
            try std.testing.expectEqual(@as(usize, 2), s.fields.len);
            try std.testing.expectEqualStrings("x", s.fields[0]);
            try std.testing.expectEqualStrings("y", s.fields[1]);
        },
        else => return error.TestUnexpectedResult,
    }

    // let p = Point { x: 1, y: 2 };
    const stmt2 = program.statements[1];
    switch (stmt2) {
        .let => |l| {
            try std.testing.expectEqualStrings("p", l.name);
            switch (l.value.*) {
                .struct_literal => |sl| {
                    try std.testing.expectEqualStrings("Point", sl.struct_name);
                    try std.testing.expectEqual(@as(usize, 2), sl.fields.len);
                    try std.testing.expectEqualStrings("x", sl.fields[0].name);
                    try std.testing.expectEqualStrings("y", sl.fields[1].name);
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }

    // print(p.x);
    const stmt3 = program.statements[2];
    switch (stmt3) {
        .print => |expr| {
            switch (expr.*) {
                .field_access => |fa| {
                    try std.testing.expectEqualStrings("x", fa.field);
                    switch (fa.object.*) {
                        .identifier => |id| try std.testing.expectEqualStrings("p", id),
                        else => return error.TestUnexpectedResult,
                    }
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }
}
