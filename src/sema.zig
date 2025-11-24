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

const StructDef = struct {
    fields: std.StringHashMap(Ast.Type),
    field_order: []const []const u8,
};

pub const Sema = struct {
    allocator: std.mem.Allocator,
    current_scope: *Scope,
    structs: std.StringHashMap(StructDef),

    pub fn init(allocator: std.mem.Allocator) !Sema {
        const root = try allocator.create(Scope);
        root.* = Scope.init(allocator, null);
        return .{
            .allocator = allocator,
            .current_scope = root,
            .structs = std.StringHashMap(StructDef).init(allocator),
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
        var it = self.structs.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.fields.deinit();
            // field_order is allocated in AST or arena, so we don't free it here if we assume arena
        }
        self.structs.deinit();
    }

    fn typesEqual(self: *Sema, t1: Ast.Type, t2: Ast.Type) bool {
        _ = self;
        switch (t1) {
            .struct_type => |n1| {
                switch (t2) {
                    .struct_type => |n2| return std.mem.eql(u8, n1, n2),
                    else => return false,
                }
            },
            else => return std.meta.eql(t1, t2),
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
                    if (!self.typesEqual(info.type, type_info)) {
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
            .array_assign => |aa| {
                const index_type = try self.analyzeExpr(aa.index);
                if (!self.typesEqual(index_type, .int)) return error.TypeMismatch;
                const value_type = try self.analyzeExpr(aa.value);
                if (!self.typesEqual(value_type, .int)) return error.TypeMismatch;

                if (self.current_scope.get(aa.name)) |info| {
                    if (!self.typesEqual(info.type, .array_int)) return error.TypeMismatch;
                } else {
                    return error.UndefinedVariable;
                }
            },
            .struct_decl => |s| {
                var fields = std.StringHashMap(Ast.Type).init(self.allocator);
                for (s.fields) |field_name| {
                    try fields.put(field_name, .int); // Default to int for now
                }
                try self.structs.put(s.name, .{ .fields = fields, .field_order = s.fields });
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
                if (!self.typesEqual(cond_type, .bool)) return error.TypeMismatch;
                try self.analyzeStmt(if_s.then_branch.*);
                if (if_s.else_branch) |else_branch| {
                    try self.analyzeStmt(else_branch.*);
                }
            },
            .while_stmt => |while_s| {
                const cond_type = try self.analyzeExpr(while_s.condition);
                if (!self.typesEqual(cond_type, .bool)) return error.TypeMismatch;
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
                    if (!self.typesEqual(cond_type, .bool)) return error.TypeMismatch;
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
                        if (!self.typesEqual(left, .int) or !self.typesEqual(right, .int)) return error.TypeMismatch;
                        return .int;
                    },
                    .equal_equal, .bang_equal => {
                        if (!self.typesEqual(left, right)) return error.TypeMismatch;
                        return .bool;
                    },
                    .less, .less_equal, .greater, .greater_equal => {
                        if (!self.typesEqual(left, .int) or !self.typesEqual(right, .int)) return error.TypeMismatch;
                        return .bool;
                    },
                    .logic_and, .logic_or => {
                        if (!self.typesEqual(left, .bool) or !self.typesEqual(right, .bool)) return error.TypeMismatch;
                        return .bool;
                    },
                }
            },
            .unary => |un| {
                const right = try self.analyzeExpr(un.right);
                switch (un.op) {
                    .bang => {
                        if (!self.typesEqual(right, .bool)) return error.TypeMismatch;
                        return .bool;
                    },
                    .minus => {
                        if (!self.typesEqual(right, .int)) return error.TypeMismatch;
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
            .array_literal => |elements| {
                for (elements) |elem| {
                    const t = try self.analyzeExpr(elem);
                    if (!self.typesEqual(t, .int)) return error.TypeMismatch;
                }
                return .array_int;
            },
            .index => |idx| {
                const callee_type = try self.analyzeExpr(idx.callee);
                if (!self.typesEqual(callee_type, .array_int)) return error.TypeMismatch;
                const index_type = try self.analyzeExpr(idx.index);
                if (!self.typesEqual(index_type, .int)) return error.TypeMismatch;
                return .int;
            },
            .struct_literal => |sl| {
                const def = self.structs.get(sl.struct_name) orelse return error.UndefinedVariable; // UndefinedType
                // Check fields
                for (sl.fields) |field| {
                    const field_type = def.fields.get(field.name) orelse return error.UndefinedVariable; // UndefinedField
                    const expr_type = try self.analyzeExpr(field.value);
                    if (!self.typesEqual(expr_type, field_type)) return error.TypeMismatch;
                }
                return Ast.Type{ .struct_type = sl.struct_name };
            },
            .field_access => |fa| {
                const obj_type = try self.analyzeExpr(fa.object);
                switch (obj_type) {
                    .struct_type => |name| {
                        const def = self.structs.get(name) orelse return error.UndefinedVariable; // Should not happen if type exists
                        const field_type = def.fields.get(fa.field) orelse return error.UndefinedVariable; // UndefinedField
                        return field_type;
                    },
                    else => return error.TypeMismatch,
                }
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

test "sema arrays" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // let a = [1, 2];
    const one = try allocator.create(Ast.Expr);
    one.* = Ast.Expr{ .number = 1 };
    const two = try allocator.create(Ast.Expr);
    two.* = Ast.Expr{ .number = 2 };
    const elements = try allocator.alloc(*Ast.Expr, 2);
    elements[0] = one;
    elements[1] = two;
    const array_lit = try allocator.create(Ast.Expr);
    array_lit.* = Ast.Expr{ .array_literal = elements };
    const stmt1 = Ast.Stmt{ .let = .{ .name = "a", .value = array_lit } };

    // a[0] = 10;
    const zero = try allocator.create(Ast.Expr);
    zero.* = Ast.Expr{ .number = 0 };
    const ten = try allocator.create(Ast.Expr);
    ten.* = Ast.Expr{ .number = 10 };
    const stmt2 = Ast.Stmt{ .array_assign = .{ .name = "a", .index = zero, .value = ten } };

    // print(a[1]);
    const ref_a = try allocator.create(Ast.Expr);
    ref_a.* = Ast.Expr{ .identifier = "a" };
    const one_idx = try allocator.create(Ast.Expr);
    one_idx.* = Ast.Expr{ .number = 1 };
    const index_expr = try allocator.create(Ast.Expr);
    index_expr.* = Ast.Expr{ .index = .{ .callee = ref_a, .index = one_idx } };
    const stmt3 = Ast.Stmt{ .print = index_expr };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{ stmt1, stmt2, stmt3 } };

    var sema = try Sema.init(allocator);
    defer sema.deinit();

    try sema.analyze(program);
}

test "sema structs" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // struct Point { x, y }
    const fields = try allocator.alloc([]const u8, 2);
    fields[0] = "x";
    fields[1] = "y";
    const stmt1 = Ast.Stmt{ .struct_decl = .{ .name = "Point", .fields = fields } };

    // let p = Point { x: 1, y: 2 };
    const one = try allocator.create(Ast.Expr);
    one.* = Ast.Expr{ .number = 1 };
    const two = try allocator.create(Ast.Expr);
    two.* = Ast.Expr{ .number = 2 };
    const struct_fields = try allocator.alloc(Ast.StructFieldInit, 2);
    struct_fields[0] = .{ .name = "x", .value = one };
    struct_fields[1] = .{ .name = "y", .value = two };
    const struct_lit = try allocator.create(Ast.Expr);
    struct_lit.* = Ast.Expr{ .struct_literal = .{ .struct_name = "Point", .fields = struct_fields } };
    const stmt2 = Ast.Stmt{ .let = .{ .name = "p", .value = struct_lit } };

    // print(p.x);
    const ref_p = try allocator.create(Ast.Expr);
    ref_p.* = Ast.Expr{ .identifier = "p" };
    const field_access = try allocator.create(Ast.Expr);
    field_access.* = Ast.Expr{ .field_access = .{ .object = ref_p, .field = "x" } };
    const stmt3 = Ast.Stmt{ .print = field_access };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{ stmt1, stmt2, stmt3 } };

    var sema = try Sema.init(allocator);
    defer sema.deinit();

    try sema.analyze(program);
}
