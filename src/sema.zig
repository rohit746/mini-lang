const std = @import("std");
const Ast = @import("ast.zig");

const SymbolInfo = struct {
    type: Ast.Type,
};

const Scope = struct {
    symbols: std.StringHashMap(SymbolInfo),
    parent: ?*Scope,

    pub fn init(allocator: std.mem.Allocator, parent: ?*Scope) Scope {
        return .{
            .symbols = std.StringHashMap(SymbolInfo).init(allocator),
            .parent = parent,
        };
    }

    pub fn deinit(self: *Scope) void {
        self.symbols.deinit();
    }

    pub fn put(self: *Scope, name: []const u8, type_info: Ast.Type) !void {
        try self.symbols.put(name, .{ .type = type_info });
    }

    pub fn get(self: *Scope, name: []const u8) ?SymbolInfo {
        if (self.symbols.get(name)) |info| return info;
        if (self.parent) |p| return p.get(name);
        return null;
    }

    pub fn contains(self: *Scope, name: []const u8) bool {
        return self.get(name) != null;
    }
};

pub const Sema = struct {
    allocator: std.mem.Allocator,
    current_scope: *Scope,

    pub fn init(allocator: std.mem.Allocator) !Sema {
        const root = try allocator.create(Scope);
        root.* = Scope.init(allocator, null);
        return .{
            .allocator = allocator,
            .current_scope = root,
        };
    }

    pub fn deinit(self: *Sema) void {
        var scope: ?*Scope = self.current_scope;
        while (scope) |s| {
            const parent = s.parent;
            s.deinit();
            self.allocator.destroy(s);
            scope = parent;
        }
    }

    fn enterScope(self: *Sema) !void {
        const new_scope = try self.allocator.create(Scope);
        new_scope.* = Scope.init(self.allocator, self.current_scope);
        self.current_scope = new_scope;
    }

    fn leaveScope(self: *Sema) void {
        if (self.current_scope.parent) |parent| {
            const old_scope = self.current_scope;
            old_scope.deinit();
            self.allocator.destroy(old_scope);
            self.current_scope = parent;
        }
    }

    pub fn analyze(self: *Sema, program: Ast.Program) !void {
        for (program.statements) |stmt| {
            try self.analyzeStmt(stmt);
        }
    }

    fn analyzeStmt(self: *Sema, stmt: Ast.Stmt) !void {
        switch (stmt) {
            .let => |decl| {
                const type_info = try self.analyzeExpr(decl.value);
                try self.current_scope.put(decl.name, type_info);
            },
            .assign => |assign| {
                const type_info = try self.analyzeExpr(assign.value);
                if (self.current_scope.get(assign.name)) |info| {
                    if (info.type != type_info) {
                        // For now, strict type checking on assignment?
                        // Or allow int <-> string? No.
                        // But we might have unknown types if we had them.
                        // For now, let's enforce it.
                        return error.TypeMismatch;
                    }
                } else {
                    return error.UndefinedVariable;
                }
            },
            .print => |expr| {
                _ = try self.analyzeExpr(expr);
            },
            .block => |stmts| {
                try self.enterScope();
                for (stmts) |s| {
                    try self.analyzeStmt(s);
                }
                self.leaveScope();
            },
            .if_stmt => |if_s| {
                const cond_type = try self.analyzeExpr(if_s.condition);
                if (cond_type != .bool) return error.TypeMismatch;
                try self.analyzeStmt(if_s.then_branch.*);
                if (if_s.else_branch) |else_branch| {
                    try self.analyzeStmt(else_branch.*);
                }
            },
            .while_stmt => |while_s| {
                const cond_type = try self.analyzeExpr(while_s.condition);
                if (cond_type != .bool) return error.TypeMismatch;
                try self.analyzeStmt(while_s.body.*);
            },
            .fn_decl => |func| {
                // Register function name in current scope (usually global)
                try self.current_scope.put(func.name, .void); // Functions are void for now (or we could have a function type)

                // Enter new scope for function body
                try self.enterScope();

                // Add parameters to scope
                for (func.params) |param| {
                    try self.current_scope.put(param, .int); // Assume int for parameters
                }

                // Analyze body
                try self.analyzeStmt(func.body.*);

                self.leaveScope();
            },
            .return_stmt => |ret| {
                if (ret) |expr| {
                    _ = try self.analyzeExpr(expr);
                }
            },
            .for_stmt => |for_s| {
                try self.enterScope();
                if (for_s.init) |init_s| {
                    try self.analyzeStmt(init_s.*);
                }
                if (for_s.condition) |cond| {
                    const cond_type = try self.analyzeExpr(cond);
                    if (cond_type != .bool) return error.TypeMismatch;
                }
                if (for_s.increment) |incr| {
                    try self.analyzeStmt(incr.*);
                }
                try self.analyzeStmt(for_s.body.*);
                self.leaveScope();
            },
        }
    }

    fn analyzeExpr(self: *Sema, expr: *Ast.Expr) !Ast.Type {
        switch (expr.*) {
            .number => return .int,
            .boolean => return .bool,
            .string => return .string,
            .identifier => |name| {
                if (self.current_scope.get(name)) |info| {
                    return info.type;
                } else {
                    return error.UndefinedVariable;
                }
            },
            .binary => |bin| {
                const left = try self.analyzeExpr(bin.left);
                const right = try self.analyzeExpr(bin.right);

                switch (bin.op) {
                    .add, .sub, .mul, .div => {
                        if (left != .int or right != .int) return error.TypeMismatch;
                        return .int;
                    },
                    .equal_equal, .bang_equal => {
                        if (left != right) return error.TypeMismatch;
                        return .bool;
                    },
                    .less, .less_equal, .greater, .greater_equal => {
                        if (left != .int or right != .int) return error.TypeMismatch;
                        return .bool;
                    },
                    .logic_and, .logic_or => {
                        if (left != .bool or right != .bool) return error.TypeMismatch;
                        return .bool;
                    },
                }
            },
            .unary => |un| {
                const right = try self.analyzeExpr(un.right);
                switch (un.op) {
                    .bang => {
                        if (right != .bool) return error.TypeMismatch;
                        return .bool;
                    },
                    .minus => {
                        if (right != .int) return error.TypeMismatch;
                        return .int;
                    },
                }
            },
            .call => |call| {
                if (!self.current_scope.contains(call.callee)) {
                    return error.UndefinedFunction;
                }
                for (call.args) |arg| {
                    _ = try self.analyzeExpr(arg);
                }
                // Assume functions return int
                return .int;
            },
        }
    }
};

pub const ParseError = error{
    UnexpectedToken,
    ExpectedStatement,
    ExpectedIdentifier,
    OutOfMemory,
    InvalidCharacter,
    Overflow,
    UndefinedFunction,
    TypeMismatch,
    UndefinedVariable,
};

test "sema basic" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // let x = 10;
    const val = try allocator.create(Ast.Expr);
    val.* = Ast.Expr{ .number = 10 };
    const stmt1 = Ast.Stmt{ .let = .{ .name = "x", .value = val } };

    // print(x);
    const ref = try allocator.create(Ast.Expr);
    ref.* = Ast.Expr{ .identifier = "x" };
    const stmt2 = Ast.Stmt{ .print = ref };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{ stmt1, stmt2 } };

    var sema = try Sema.init(allocator);
    defer sema.deinit();

    try sema.analyze(program);
}

test "sema undefined variable" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // print(y);
    const ref = try allocator.create(Ast.Expr);
    ref.* = Ast.Expr{ .identifier = "y" };
    const stmt = Ast.Stmt{ .print = ref };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{stmt} };

    var sema = try Sema.init(allocator);
    defer sema.deinit();

    try std.testing.expectError(error.UndefinedVariable, sema.analyze(program));
}

test "sema scopes" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // let x = 10;
    const val = try allocator.create(Ast.Expr);
    val.* = Ast.Expr{ .number = 10 };
    const stmt1 = Ast.Stmt{ .let = .{ .name = "x", .value = val } };

    // { let y = 20; print(x); print(y); }
    const val2 = try allocator.create(Ast.Expr);
    val2.* = Ast.Expr{ .number = 20 };
    const stmt_inner_let = Ast.Stmt{ .let = .{ .name = "y", .value = val2 } };

    const ref_x = try allocator.create(Ast.Expr);
    ref_x.* = Ast.Expr{ .identifier = "x" };
    const stmt_print_x = Ast.Stmt{ .print = ref_x };

    const ref_y = try allocator.create(Ast.Expr);
    ref_y.* = Ast.Expr{ .identifier = "y" };
    const stmt_print_y = Ast.Stmt{ .print = ref_y };

    const block_stmts = try allocator.alloc(Ast.Stmt, 3);
    block_stmts[0] = stmt_inner_let;
    block_stmts[1] = stmt_print_x;
    block_stmts[2] = stmt_print_y;
    const stmt_block = Ast.Stmt{ .block = block_stmts };

    // print(y); // Should fail
    const ref_y_outer = try allocator.create(Ast.Expr);
    ref_y_outer.* = Ast.Expr{ .identifier = "y" };
    const stmt_fail = Ast.Stmt{ .print = ref_y_outer };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{ stmt1, stmt_block } };
    const program_fail = Ast.Program{ .statements = &[_]Ast.Stmt{ stmt1, stmt_block, stmt_fail } };

    var sema = try Sema.init(allocator);
    defer sema.deinit();

    try sema.analyze(program);

    var sema2 = try Sema.init(allocator);
    defer sema2.deinit();
    try std.testing.expectError(error.UndefinedVariable, sema2.analyze(program_fail));
}

test "sema type mismatch" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // let x = "hello";
    const val = try allocator.create(Ast.Expr);
    val.* = Ast.Expr{ .string = "hello" };
    const stmt1 = Ast.Stmt{ .let = .{ .name = "x", .value = val } };

    // x + 1
    const ref_x = try allocator.create(Ast.Expr);
    ref_x.* = Ast.Expr{ .identifier = "x" };
    const one = try allocator.create(Ast.Expr);
    one.* = Ast.Expr{ .number = 1 };
    const bin = try allocator.create(Ast.Expr);
    bin.* = Ast.Expr{ .binary = .{ .left = ref_x, .op = .add, .right = one } };
    const stmt2 = Ast.Stmt{ .print = bin };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{ stmt1, stmt2 } };

    var sema = try Sema.init(allocator);
    defer sema.deinit();

    try std.testing.expectError(error.TypeMismatch, sema.analyze(program));
}

test "sema for loop" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // for (let i = 0; i < 10; i = i + 1) { print(i); }

    // Init: let i = 0;
    const val_0 = try allocator.create(Ast.Expr);
    val_0.* = Ast.Expr{ .number = 0 };
    const init_stmt = try allocator.create(Ast.Stmt);
    init_stmt.* = Ast.Stmt{ .let = .{ .name = "i", .value = val_0 } };

    // Cond: i < 10
    const ref_i = try allocator.create(Ast.Expr);
    ref_i.* = Ast.Expr{ .identifier = "i" };
    const val_10 = try allocator.create(Ast.Expr);
    val_10.* = Ast.Expr{ .number = 10 };
    const cond_expr = try allocator.create(Ast.Expr);
    cond_expr.* = Ast.Expr{ .binary = .{ .left = ref_i, .op = .less, .right = val_10 } };

    // Incr: i = i + 1
    const ref_i_2 = try allocator.create(Ast.Expr);
    ref_i_2.* = Ast.Expr{ .identifier = "i" };
    const val_1 = try allocator.create(Ast.Expr);
    val_1.* = Ast.Expr{ .number = 1 };
    const add_expr = try allocator.create(Ast.Expr);
    add_expr.* = Ast.Expr{ .binary = .{ .left = ref_i_2, .op = .add, .right = val_1 } };
    const incr_stmt = try allocator.create(Ast.Stmt);
    incr_stmt.* = Ast.Stmt{ .assign = .{ .name = "i", .value = add_expr } };

    // Body: print(i)
    const ref_i_3 = try allocator.create(Ast.Expr);
    ref_i_3.* = Ast.Expr{ .identifier = "i" };
    const body_stmt = try allocator.create(Ast.Stmt);
    body_stmt.* = Ast.Stmt{ .print = ref_i_3 };

    const for_stmt = Ast.Stmt{ .for_stmt = .{
        .init = init_stmt,
        .condition = cond_expr,
        .increment = incr_stmt,
        .body = body_stmt,
    } };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{for_stmt} };

    var sema = try Sema.init(allocator);
    defer sema.deinit();

    try sema.analyze(program);
}
