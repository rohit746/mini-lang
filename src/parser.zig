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
        const start_loc = self.current_token.loc;
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
            const name = self.lexer.source[start_loc.start..start_loc.end];
            self.advance(); // Eat identifier

            if (self.current_token.tag == .equal) {
                self.advance(); // Eat `=`
                const value = try self.parseExpression();
                try self.eat(.semicolon);
                return Ast.Stmt{ .loc = start_loc, .data = .{ .assign = .{ .name = name, .value = value } } };
            } else if (self.current_token.tag == .l_bracket) {
                self.advance(); // Eat `[`
                const index = try self.parseExpression();
                try self.eat(.r_bracket);
                try self.eat(.equal);
                const value = try self.parseExpression();
                try self.eat(.semicolon);
                return Ast.Stmt{ .loc = start_loc, .data = .{ .array_assign = .{ .name = name, .index = index, .value = value } } };
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
                call_expr.* = Ast.Expr{ .loc = start_loc, .data = .{ .call = .{ .callee = name, .args = try args.toOwnedSlice(self.allocator) } } };

                // Wait, call is an Expr, but here we are in parseStatement.
                // We need to wrap it in an ExprStmt?
                // The current AST doesn't have ExprStmt.
                // The original code returned error.ExpectedStatement here!
                // "else { return error.ExpectedStatement; }"
                // Ah, the original code didn't support standalone function calls as statements?
                // Let's check the original code.

                // Original:
                // } else if (self.current_token.tag == .l_paren) {
                //     try self.eat(.l_paren);
                //     ...
                //     const call_expr = ...;
                //     call_expr.* = Ast.Expr{ .call = ... };
                //     return error.ExpectedStatement;
                // }

                // Yes, it returned error.ExpectedStatement. It seems the language doesn't support expression statements yet.
                // But wait, `print(x)` is a statement.
                // `add(1, 2);` should be valid if the return value is ignored.
                // But the parser explicitly rejected it.
                // I will keep the behavior for now.
                return error.ExpectedStatement;
            } else {
                return error.ExpectedStatement;
            }
        } else {
            return error.ExpectedStatement;
        }
    }

    fn parseFnDecl(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
        try self.eat(.keyword_fn);

        if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
        const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
        self.advance();

        try self.eat(.l_paren);
        var params = std.ArrayListUnmanaged(Ast.FnParam){};
        errdefer params.deinit(self.allocator);

        if (self.current_token.tag != .r_paren) {
            while (true) {
                if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
                const param_name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
                self.advance();

                try self.eat(.colon);
                const param_type = try self.parseType();

                try params.append(self.allocator, .{ .name = param_name, .type = param_type });

                if (self.current_token.tag == .comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        try self.eat(.r_paren);

        var return_type: Ast.Type = .void;
        if (self.current_token.tag == .arrow) {
            self.advance();
            return_type = try self.parseType();
        }

        const body = try self.allocator.create(Ast.Stmt);
        body.* = try self.parseBlock();

        return Ast.Stmt{ .loc = start_loc, .data = .{ .fn_decl = .{ .name = name, .params = try params.toOwnedSlice(self.allocator), .return_type = return_type, .body = body } } };
    }

    fn parseReturnStmt(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
        try self.eat(.keyword_return);
        var value: ?*Ast.Expr = null;
        if (self.current_token.tag != .semicolon) {
            value = try self.parseExpression();
        }
        try self.eat(.semicolon);
        return Ast.Stmt{ .loc = start_loc, .data = .{ .return_stmt = value } };
    }

    fn parseBlock(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
        try self.eat(.l_brace);
        var statements = std.ArrayListUnmanaged(Ast.Stmt){};
        errdefer statements.deinit(self.allocator);

        while (self.current_token.tag != .r_brace and self.current_token.tag != .eof) {
            const stmt = try self.parseStatement();
            try statements.append(self.allocator, stmt);
        }
        try self.eat(.r_brace);

        return Ast.Stmt{ .loc = start_loc, .data = .{ .block = try statements.toOwnedSlice(self.allocator) } };
    }

    fn parseIfStmt(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
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

        return Ast.Stmt{ .loc = start_loc, .data = .{ .if_stmt = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch } } };
    }

    fn parseWhileStmt(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
        try self.eat(.keyword_while);
        try self.eat(.l_paren);
        const condition = try self.parseExpression();
        try self.eat(.r_paren);

        const body = try self.allocator.create(Ast.Stmt);
        body.* = try self.parseStatement();

        return Ast.Stmt{ .loc = start_loc, .data = .{ .while_stmt = .{ .condition = condition, .body = body } } };
    }

    fn parseForStmt(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
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
                const incr_start_loc = self.current_token.loc;
                const name = self.lexer.source[incr_start_loc.start..incr_start_loc.end];
                self.advance();

                if (self.current_token.tag == .equal) {
                    self.advance();
                    const value = try self.parseExpression();
                    increment = try self.allocator.create(Ast.Stmt);
                    increment.?.* = Ast.Stmt{ .loc = incr_start_loc, .data = .{ .assign = .{ .name = name, .value = value } } };
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

        return Ast.Stmt{ .loc = start_loc, .data = .{ .for_stmt = .{ .init = init_stmt, .condition = condition, .increment = increment, .body = body } } };
    }

    fn parseAssignment(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
        const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
        try self.eat(.identifier);
        try self.eat(.equal);
        const value = try self.parseExpression();
        try self.eat(.semicolon);
        return Ast.Stmt{ .loc = start_loc, .data = .{ .assign = .{ .name = name, .value = value } } };
    }

    fn parseType(self: *Parser) ParseError!Ast.Type {
        if (self.current_token.tag == .identifier) {
            const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
            self.advance();
            if (std.mem.eql(u8, name, "int")) return .int;
            if (std.mem.eql(u8, name, "string")) return .string;
            if (std.mem.eql(u8, name, "bool")) return .bool;
            if (std.mem.eql(u8, name, "void")) return .void;
            return Ast.Type{ .struct_type = name };
        } else if (self.current_token.tag == .l_bracket) {
            self.advance(); // [
            try self.eat(.r_bracket); // ]
            const elem_type = try self.parseType();
            if (elem_type == .int) return .array_int;
            return error.UnexpectedToken;
        }
        return error.UnexpectedToken;
    }

    fn parseVarDecl(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
        try self.eat(.keyword_let);

        if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
        const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
        self.advance();

        var type_annotation: ?Ast.Type = null;
        if (self.current_token.tag == .colon) {
            self.advance();
            type_annotation = try self.parseType();
        }

        try self.eat(.equal);
        const value = try self.parseExpression();
        try self.eat(.semicolon);

        return Ast.Stmt{ .loc = start_loc, .data = .{ .let = .{ .name = name, .type = type_annotation, .value = value } } };
    }

    fn parsePrintStmt(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
        try self.eat(.keyword_print);
        try self.eat(.l_paren);
        const expr = try self.parseExpression();
        try self.eat(.r_paren);
        try self.eat(.semicolon);
        return Ast.Stmt{ .loc = start_loc, .data = .{ .print = expr } };
    }

    fn parseExpression(self: *Parser) ParseError!*Ast.Expr {
        return self.parseLogicOr();
    }

    fn parseLogicOr(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseLogicAnd();

        while (self.current_token.tag == .pipe_pipe) {
            const op_loc = self.current_token.loc;
            self.advance();
            const right = try self.parseLogicAnd();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .loc = op_loc, .data = .{ .binary = .{ .left = left, .op = .logic_or, .right = right } } };
            left = new_node;
        }

        return left;
    }

    fn parseLogicAnd(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseEquality();

        while (self.current_token.tag == .ampersand_ampersand) {
            const op_loc = self.current_token.loc;
            self.advance();
            const right = try self.parseEquality();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .loc = op_loc, .data = .{ .binary = .{ .left = left, .op = .logic_and, .right = right } } };
            left = new_node;
        }

        return left;
    }

    fn parseEquality(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseComparison();

        while (self.current_token.tag == .equal_equal or self.current_token.tag == .bang_equal) {
            const op_loc = self.current_token.loc;
            const op: Ast.BinaryOp = if (self.current_token.tag == .equal_equal) .equal_equal else .bang_equal;
            self.advance();
            const right = try self.parseComparison();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .loc = op_loc, .data = .{ .binary = .{ .left = left, .op = op, .right = right } } };
            left = new_node;
        }

        return left;
    }

    fn parseComparison(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseTerm();

        while (self.current_token.tag == .less or self.current_token.tag == .less_equal or
            self.current_token.tag == .greater or self.current_token.tag == .greater_equal)
        {
            const op_loc = self.current_token.loc;
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
            new_node.* = Ast.Expr{ .loc = op_loc, .data = .{ .binary = .{ .left = left, .op = op, .right = right } } };
            left = new_node;
        }

        return left;
    }

    fn parseTerm(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseFactor();

        while (self.current_token.tag == .plus or self.current_token.tag == .minus) {
            const op_loc = self.current_token.loc;
            const op: Ast.BinaryOp = if (self.current_token.tag == .plus) .add else .sub;
            self.advance();
            const right = try self.parseFactor();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .loc = op_loc, .data = .{ .binary = .{ .left = left, .op = op, .right = right } } };
            left = new_node;
        }

        return left;
    }

    fn parseFactor(self: *Parser) ParseError!*Ast.Expr {
        var left = try self.parseUnary();

        while (self.current_token.tag == .star or self.current_token.tag == .slash) {
            const op_loc = self.current_token.loc;
            const op: Ast.BinaryOp = if (self.current_token.tag == .star) .mul else .div;
            self.advance();
            const right = try self.parseUnary();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .loc = op_loc, .data = .{ .binary = .{ .left = left, .op = op, .right = right } } };
            left = new_node;
        }

        return left;
    }

    fn parseUnary(self: *Parser) ParseError!*Ast.Expr {
        if (self.current_token.tag == .bang or self.current_token.tag == .minus) {
            const op_loc = self.current_token.loc;
            const op: Ast.UnaryOp = if (self.current_token.tag == .bang) .bang else .minus;
            self.advance();
            const right = try self.parseUnary();

            const new_node = try self.allocator.create(Ast.Expr);
            new_node.* = Ast.Expr{ .loc = op_loc, .data = .{ .unary = .{ .op = op, .right = right } } };
            return new_node;
        }
        return self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!*Ast.Expr {
        var expr: *Ast.Expr = undefined;
        const start_loc = self.current_token.loc;

        if (self.current_token.tag == .number_literal) {
            const text = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
            const num = try std.fmt.parseInt(i64, text, 10);
            self.advance();
            const node = try self.allocator.create(Ast.Expr);
            node.* = Ast.Expr{ .loc = start_loc, .data = .{ .number = num } };
            expr = node;
        } else if (self.current_token.tag == .string_literal) {
            // Strip quotes
            const raw = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
            const content = raw[1 .. raw.len - 1];
            self.advance();
            const node = try self.allocator.create(Ast.Expr);
            node.* = Ast.Expr{ .loc = start_loc, .data = .{ .string = content } };
            expr = node;
        } else if (self.current_token.tag == .keyword_true) {
            self.advance();
            const node = try self.allocator.create(Ast.Expr);
            node.* = Ast.Expr{ .loc = start_loc, .data = .{ .boolean = true } };
            expr = node;
        } else if (self.current_token.tag == .keyword_false) {
            self.advance();
            const node = try self.allocator.create(Ast.Expr);
            node.* = Ast.Expr{ .loc = start_loc, .data = .{ .boolean = false } };
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
                node.* = Ast.Expr{ .loc = start_loc, .data = .{ .call = .{ .callee = name, .args = try args.toOwnedSlice(self.allocator) } } };
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
                node.* = Ast.Expr{ .loc = start_loc, .data = .{ .struct_literal = .{ .struct_name = name, .fields = try fields.toOwnedSlice(self.allocator) } } };
                expr = node;
            } else {
                const node = try self.allocator.create(Ast.Expr);
                node.* = Ast.Expr{ .loc = start_loc, .data = .{ .identifier = name } };
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
            node.* = Ast.Expr{ .loc = start_loc, .data = .{ .array_literal = try elements.toOwnedSlice(self.allocator) } };
            expr = node;
        } else {
            return error.UnexpectedToken;
        }

        // Postfix operators (indexing, field access)
        while (true) {
            if (self.current_token.tag == .l_bracket) {
                const op_loc = self.current_token.loc;
                self.advance(); // eat [
                const index_expr = try self.parseExpression();
                try self.eat(.r_bracket);
                const node = try self.allocator.create(Ast.Expr);
                node.* = Ast.Expr{ .loc = op_loc, .data = .{ .index = .{ .callee = expr, .index = index_expr } } };
                expr = node;
            } else if (self.current_token.tag == .dot) {
                const op_loc = self.current_token.loc;
                self.advance(); // eat .
                if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
                const field_name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
                self.advance();
                const node = try self.allocator.create(Ast.Expr);
                node.* = Ast.Expr{ .loc = op_loc, .data = .{ .field_access = .{ .object = expr, .field = field_name } } };
                expr = node;
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseStructDecl(self: *Parser) ParseError!Ast.Stmt {
        const start_loc = self.current_token.loc;
        try self.eat(.keyword_struct);

        if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
        const name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
        self.advance();

        try self.eat(.l_brace);
        var fields = std.ArrayListUnmanaged(Ast.StructField){};
        errdefer fields.deinit(self.allocator);

        if (self.current_token.tag != .r_brace) {
            while (true) {
                if (self.current_token.tag != .identifier) return error.ExpectedIdentifier;
                const field_name = self.lexer.source[self.current_token.loc.start..self.current_token.loc.end];
                self.advance();

                try self.eat(.colon);
                const field_type = try self.parseType();

                try fields.append(self.allocator, .{ .name = field_name, .type = field_type });

                if (self.current_token.tag == .comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        try self.eat(.r_brace);

        return Ast.Stmt{ .loc = start_loc, .data = .{ .struct_decl = .{ .name = name, .fields = try fields.toOwnedSlice(self.allocator) } } };
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
    try std.testing.expect(stmt.data == .if_stmt);
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
    switch (stmt.data) {
        .let => |decl| {
            try std.testing.expectEqualStrings("s", decl.name);
            switch (decl.value.data) {
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
    switch (stmt.data) {
        .let => |decl| {
            try std.testing.expectEqualStrings("b", decl.name);
            // Check structure: (true && false) || true
            switch (decl.value.data) {
                .binary => |bin| {
                    try std.testing.expectEqual(Ast.BinaryOp.logic_or, bin.op);
                    switch (bin.left.data) {
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
    switch (stmt.data) {
        .for_stmt => |f| {
            // Check init
            if (f.init) |init_stmt| {
                switch (init_stmt.data) {
                    .let => |l| {
                        try std.testing.expectEqualStrings("i", l.name);
                    },
                    else => return error.TestUnexpectedResult,
                }
            } else return error.TestUnexpectedResult;

            // Check condition
            if (f.condition) |cond| {
                switch (cond.data) {
                    .binary => |bin| {
                        try std.testing.expectEqual(Ast.BinaryOp.less, bin.op);
                    },
                    else => return error.TestUnexpectedResult,
                }
            } else return error.TestUnexpectedResult;

            // Check increment
            if (f.increment) |incr| {
                switch (incr.data) {
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
    switch (stmt1.data) {
        .let => |l| {
            try std.testing.expectEqualStrings("a", l.name);
            switch (l.value.data) {
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
    switch (stmt2.data) {
        .let => |l| {
            try std.testing.expectEqualStrings("b", l.name);
            switch (l.value.data) {
                .index => |idx| {
                    switch (idx.callee.data) {
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
    switch (stmt3.data) {
        .array_assign => |aa| {
            try std.testing.expectEqualStrings("a", aa.name);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "parser structs" {
    const source =
        \\struct Point { x: int, y: int }
        \\let p = Point { x: 1, y: 2 };
        \\print(p.x);
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 3), program.statements.len);

    // struct Point { x: int, y: int }
    const stmt1 = program.statements[0];
    switch (stmt1.data) {
        .struct_decl => |s| {
            try std.testing.expectEqualStrings("Point", s.name);
            try std.testing.expectEqual(@as(usize, 2), s.fields.len);
            try std.testing.expectEqualStrings("x", s.fields[0].name);
            try std.testing.expectEqual(Ast.Type.int, s.fields[0].type);
            try std.testing.expectEqualStrings("y", s.fields[1].name);
            try std.testing.expectEqual(Ast.Type.int, s.fields[1].type);
        },
        else => return error.TestUnexpectedResult,
    }

    // let p = Point { x: 1, y: 2 };
    const stmt2 = program.statements[1];
    switch (stmt2.data) {
        .let => |l| {
            try std.testing.expectEqualStrings("p", l.name);
            switch (l.value.data) {
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
    switch (stmt3.data) {
        .print => |expr| {
            switch (expr.data) {
                .field_access => |fa| {
                    try std.testing.expectEqualStrings("x", fa.field);
                    switch (fa.object.data) {
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

test "parser explicit types" {
    const source =
        \\let x: int = 10;
        \\fn add(a: int, b: int) -> int {
        \\    return a + b;
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    try std.testing.expectEqual(@as(usize, 2), program.statements.len);

    // let x: int = 10;
    const stmt1 = program.statements[0];
    switch (stmt1.data) {
        .let => |l| {
            try std.testing.expectEqualStrings("x", l.name);
            try std.testing.expectEqual(Ast.Type.int, l.type.?);
        },
        else => return error.TestUnexpectedResult,
    }

    // fn add(a: int, b: int) -> int { ... }
    const stmt2 = program.statements[1];
    switch (stmt2.data) {
        .fn_decl => |f| {
            try std.testing.expectEqualStrings("add", f.name);
            try std.testing.expectEqual(@as(usize, 2), f.params.len);
            try std.testing.expectEqualStrings("a", f.params[0].name);
            try std.testing.expectEqual(Ast.Type.int, f.params[0].type);
            try std.testing.expectEqualStrings("b", f.params[1].name);
            try std.testing.expectEqual(Ast.Type.int, f.params[1].type);
            try std.testing.expectEqual(Ast.Type.int, f.return_type);
        },
        else => return error.TestUnexpectedResult,
    }
}
