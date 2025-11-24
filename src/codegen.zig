const std = @import("std");
const Ast = @import("ast.zig");

const SymbolInfo = struct {
    offset: usize,
    type: Ast.Type,
};

const Scope = struct {
    vars: std.StringHashMap(SymbolInfo),
    parent: ?*Scope,

    pub fn init(allocator: std.mem.Allocator, parent: ?*Scope) Scope {
        return .{
            .vars = std.StringHashMap(SymbolInfo).init(allocator),
            .parent = parent,
        };
    }

    pub fn deinit(self: *Scope) void {
        self.vars.deinit();
    }

    pub fn put(self: *Scope, name: []const u8, offset: usize, type_info: Ast.Type) !void {
        try self.vars.put(name, .{ .offset = offset, .type = type_info });
    }

    pub fn get(self: *Scope, name: []const u8) ?SymbolInfo {
        if (self.vars.get(name)) |info| return info;
        if (self.parent) |p| return p.get(name);
        return null;
    }
};

pub const Codegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayListUnmanaged(u8),
    stack_offset: usize,
    current_scope: *Scope,
    label_counter: usize,
    string_literals: std.StringHashMap(usize),

    pub fn init(allocator: std.mem.Allocator) !Codegen {
        const root = try allocator.create(Scope);
        root.* = Scope.init(allocator, null);
        return .{
            .allocator = allocator,
            .output = .{},
            .stack_offset = 0,
            .current_scope = root,
            .label_counter = 0,
            .string_literals = std.StringHashMap(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Codegen) void {
        self.output.deinit(self.allocator);
        self.string_literals.deinit();
        var scope: ?*Scope = self.current_scope;
        while (scope) |s| {
            const parent = s.parent;
            s.deinit();
            self.allocator.destroy(s);
            scope = parent;
        }
    }

    fn enterScope(self: *Codegen) !void {
        const new_scope = try self.allocator.create(Scope);
        new_scope.* = Scope.init(self.allocator, self.current_scope);
        self.current_scope = new_scope;
    }

    fn leaveScope(self: *Codegen) void {
        if (self.current_scope.parent) |parent| {
            const old_scope = self.current_scope;
            old_scope.deinit();
            self.allocator.destroy(old_scope);
            self.current_scope = parent;
        }
    }

    fn newLabel(self: *Codegen) usize {
        const label = self.label_counter;
        self.label_counter += 1;
        return label;
    }

    fn emit(self: *Codegen, comptime format: []const u8, args: anytype) !void {
        try self.output.writer(self.allocator).print(format, args);
    }

    pub fn generate(self: *Codegen, program: Ast.Program) ![]const u8 {
        // Header
        try self.emit(".global _main\n", .{});
        try self.emit(".text\n", .{});

        // Entry point
        try self.emit("_main:\n", .{});
        try self.emit("  push %rbp\n", .{});
        try self.emit("  mov %rsp, %rbp\n", .{});
        try self.emit("  sub $256, %rsp\n", .{}); // Reserve 256 bytes for locals
        try self.emit("  push %rbx\n", .{}); // Save callee-saved register

        for (program.statements) |stmt| {
            try self.genStmt(stmt);
        }

        // Exit
        try self.emit("  mov $0, %rax\n", .{});
        try self.emit("  pop %rbx\n", .{}); // Restore callee-saved register
        try self.emit("  leave\n", .{});
        try self.emit("  ret\n", .{});

        // String literals
        if (self.string_literals.count() > 0) {
            try self.emit(".data\n", .{});
            var it = self.string_literals.iterator();
            while (it.next()) |entry| {
                try self.emit("L_str_{d}: .asciz \"{s}\"\n", .{ entry.value_ptr.*, entry.key_ptr.* });
            }
        }

        return self.output.toOwnedSlice(self.allocator);
    }

    fn getExprType(self: *Codegen, expr: *Ast.Expr) !Ast.Type {
        switch (expr.*) {
            .number => return .int,
            .boolean => return .bool,
            .string => return .string,
            .identifier => |name| {
                const info = self.current_scope.get(name) orelse return error.UnknownVariable;
                return info.type;
            },
            .unary => |un| {
                switch (un.op) {
                    .bang => return .bool,
                    .minus => return .int,
                }
            },
            .binary => |bin| {
                switch (bin.op) {
                    .equal_equal, .bang_equal, .less, .less_equal, .greater, .greater_equal, .logic_and, .logic_or => return .bool,
                    else => return .int,
                }
            },
            .call => return .int, // Assume int return
        }
    }

    fn genStmt(self: *Codegen, stmt: Ast.Stmt) !void {
        switch (stmt) {
            .let => |decl| {
                try self.genExpr(decl.value);
                // Result is in %rax
                self.stack_offset += 8;
                const type_info = try self.getExprType(decl.value);
                try self.current_scope.put(decl.name, self.stack_offset, type_info);
                try self.emit("  mov %rax, -{d}(%rbp)\n", .{self.stack_offset});
            },
            .assign => |assign| {
                try self.genExpr(assign.value);
                const info = self.current_scope.get(assign.name) orelse return error.UnknownVariable;
                try self.emit("  mov %rax, -{d}(%rbp)\n", .{info.offset});
            },
            .print => |expr| {
                try self.genExpr(expr);
                // Result in %rax. Move to %rdi (1st arg) for printf
                try self.emit("  mov %rax, %rdi\n", .{});
                const type_info = try self.getExprType(expr);
                if (type_info == .int) {
                    try self.emit("  call _print_int\n", .{});
                } else if (type_info == .string) {
                    try self.emit("  call _print_string\n", .{});
                } else {
                    // Default to int for now
                    try self.emit("  call _print_int\n", .{});
                }
            },
            .block => |stmts| {
                try self.enterScope();
                for (stmts) |s| {
                    try self.genStmt(s);
                }
                self.leaveScope();
            },
            .if_stmt => |if_s| {
                const else_label = self.newLabel();
                const end_label = self.newLabel();

                try self.genExpr(if_s.condition);
                try self.emit("  cmp $0, %rax\n", .{});
                try self.emit("  je L{d}\n", .{else_label});

                try self.genStmt(if_s.then_branch.*);
                try self.emit("  jmp L{d}\n", .{end_label});

                try self.emit("L{d}:\n", .{else_label});
                if (if_s.else_branch) |else_branch| {
                    try self.genStmt(else_branch.*);
                }

                try self.emit("L{d}:\n", .{end_label});
            },
            .while_stmt => |while_s| {
                const start_label = self.newLabel();
                const end_label = self.newLabel();

                try self.emit("L{d}:\n", .{start_label});
                try self.genExpr(while_s.condition);
                try self.emit("  cmp $0, %rax\n", .{});
                try self.emit("  je L{d}\n", .{end_label});

                try self.genStmt(while_s.body.*);
                try self.emit("  jmp L{d}\n", .{start_label});

                try self.emit("L{d}:\n", .{end_label});
            },
            .fn_decl => |func| {
                // Jump over the function definition so it's not executed linearly
                const end_label = self.newLabel();
                try self.emit("  jmp L{d}\n", .{end_label});

                // Function label
                try self.emit("_{s}:\n", .{func.name});
                try self.emit("  push %rbp\n", .{});
                try self.emit("  mov %rsp, %rbp\n", .{});
                try self.emit("  sub $256, %rsp\n", .{}); // Reserve stack
                try self.emit("  push %rbx\n", .{}); // Save callee-saved
                const old_stack_offset = self.stack_offset;
                self.stack_offset = 0;

                const registers = [_][]const u8{ "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };

                for (func.params, 0..) |param, i| {
                    if (i < registers.len) {
                        self.stack_offset += 8;
                        try self.current_scope.put(param, self.stack_offset, .int); // Assume int for params
                        try self.emit("  mov {s}, -{d}(%rbp)\n", .{ registers[i], self.stack_offset });
                    } else {
                        // Stack arguments not supported yet
                        return error.TooManyArguments;
                    }
                }

                try self.genStmt(func.body.*);

                // Default return (void/0) if no return stmt hit
                try self.emit("  mov $0, %rax\n", .{});
                try self.emit("  pop %rbx\n", .{});
                try self.emit("  leave\n", .{});
                try self.emit("  ret\n", .{});

                self.leaveScope();
                self.stack_offset = old_stack_offset;

                try self.emit("L{d}:\n", .{end_label});
            },
            .return_stmt => |ret| {
                if (ret) |expr| {
                    try self.genExpr(expr);
                } else {
                    try self.emit("  mov $0, %rax\n", .{});
                }
                try self.emit("  pop %rbx\n", .{});
                try self.emit("  leave\n", .{});
                try self.emit("  ret\n", .{});
            },
        }
    }

    fn genExpr(self: *Codegen, expr: *Ast.Expr) !void {
        switch (expr.*) {
            .number => |val| {
                try self.emit("  mov ${d}, %rax\n", .{val});
            },
            .boolean => |val| {
                if (val) {
                    try self.emit("  mov $1, %rax\n", .{});
                } else {
                    try self.emit("  mov $0, %rax\n", .{});
                }
            },
            .string => |s| {
                const label = if (self.string_literals.get(s)) |l| l else blk: {
                    const l = self.newLabel();
                    try self.string_literals.put(s, l);
                    break :blk l;
                };
                try self.emit("  lea L_str_{d}(%rip), %rax\n", .{label});
            },
            .identifier => |name| {
                const info = self.current_scope.get(name) orelse return error.UnknownVariable;
                try self.emit("  mov -{d}(%rbp), %rax\n", .{info.offset});
            },
            .unary => |un| {
                try self.genExpr(un.right);
                switch (un.op) {
                    .bang => {
                        try self.emit("  cmp $0, %rax\n", .{});
                        try self.emit("  sete %al\n", .{});
                        try self.emit("  movzbq %al, %rax\n", .{});
                    },
                    .minus => {
                        try self.emit("  neg %rax\n", .{});
                    },
                }
            },
            .binary => |bin| {
                if (bin.op == .logic_and) {
                    const end_label = self.newLabel();
                    try self.genExpr(bin.left);
                    try self.emit("  cmp $0, %rax\n", .{});
                    try self.emit("  je L{d}\n", .{end_label}); // Short-circuit if false
                    try self.genExpr(bin.right);
                    try self.emit("L{d}:\n", .{end_label});
                    return;
                }
                if (bin.op == .logic_or) {
                    const end_label = self.newLabel();
                    try self.genExpr(bin.left);
                    try self.emit("  cmp $0, %rax\n", .{});
                    try self.emit("  jne L{d}\n", .{end_label}); // Short-circuit if true
                    try self.genExpr(bin.right);
                    try self.emit("L{d}:\n", .{end_label});
                    return;
                }

                try self.genExpr(bin.left);
                try self.emit("  push %rax\n", .{});

                try self.genExpr(bin.right);
                try self.emit("  mov %rax, %rbx\n", .{});

                try self.emit("  pop %rax\n", .{});

                switch (bin.op) {
                    .add => try self.emit("  add %rbx, %rax\n", .{}),
                    .sub => try self.emit("  sub %rbx, %rax\n", .{}),
                    .mul => try self.emit("  imul %rbx, %rax\n", .{}),
                    .div => {
                        try self.emit("  cqo\n", .{}); // Sign extend rax to rdx:rax
                        try self.emit("  idiv %rbx\n", .{});
                    },
                    .equal_equal => {
                        try self.emit("  cmp %rbx, %rax\n", .{});
                        try self.emit("  sete %al\n", .{});
                        try self.emit("  movzbq %al, %rax\n", .{});
                    },
                    .bang_equal => {
                        try self.emit("  cmp %rbx, %rax\n", .{});
                        try self.emit("  setne %al\n", .{});
                        try self.emit("  movzbq %al, %rax\n", .{});
                    },
                    .less => {
                        try self.emit("  cmp %rbx, %rax\n", .{});
                        try self.emit("  setl %al\n", .{});
                        try self.emit("  movzbq %al, %rax\n", .{});
                    },
                    .less_equal => {
                        try self.emit("  cmp %rbx, %rax\n", .{});
                        try self.emit("  setle %al\n", .{});
                        try self.emit("  movzbq %al, %rax\n", .{});
                    },
                    .greater => {
                        try self.emit("  cmp %rbx, %rax\n", .{});
                        try self.emit("  setg %al\n", .{});
                        try self.emit("  movzbq %al, %rax\n", .{});
                    },
                    .greater_equal => {
                        try self.emit("  cmp %rbx, %rax\n", .{});
                        try self.emit("  setge %al\n", .{});
                        try self.emit("  movzbq %al, %rax\n", .{});
                    },
                    .logic_and, .logic_or => unreachable, // Handled above
                }
            },
            .call => |call| {
                for (call.args) |arg| {
                    try self.genExpr(arg);
                    try self.emit("  push %rax\n", .{});
                }

                const registers = [_][]const u8{ "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };

                // Pop in reverse order
                var i: usize = call.args.len;
                while (i > 0) {
                    i -= 1;
                    if (i < registers.len) {
                        try self.emit("  pop {s}\n", .{registers[i]});
                    } else {
                        // Stack args not supported
                        return error.TooManyArguments;
                    }
                }

                try self.emit("  call _{s}\n", .{call.callee});
            },
        }
    }
};

test "codegen basic" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // let x = 10;
    const val = try allocator.create(Ast.Expr);
    val.* = Ast.Expr{ .number = 10 };
    const stmt1 = Ast.Stmt{ .let = .{ .name = "x", .value = val } };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{stmt1} };

    var codegen = try Codegen.init(allocator);
    defer codegen.deinit();

    const code = try codegen.generate(program);
    defer allocator.free(code);

    try std.testing.expect(std.mem.indexOf(u8, code, "mov $10, %rax") != null);
}

test "codegen function" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // fn add(a, b) { return a + b; }
    // let x = add(10, 20);

    // a + b
    const a_expr = try allocator.create(Ast.Expr);
    a_expr.* = Ast.Expr{ .identifier = "a" };
    const b_expr = try allocator.create(Ast.Expr);
    b_expr.* = Ast.Expr{ .identifier = "b" };

    const add_expr = try allocator.create(Ast.Expr);
    add_expr.* = Ast.Expr{ .binary = .{ .left = a_expr, .op = .add, .right = b_expr } };

    const ret_stmt = Ast.Stmt{ .return_stmt = add_expr };
    const body_stmt = try allocator.create(Ast.Stmt);
    body_stmt.* = Ast.Stmt{ .block = &[_]Ast.Stmt{ret_stmt} };

    const fn_decl = Ast.Stmt{ .fn_decl = .{ .name = "add", .params = &[_][]const u8{ "a", "b" }, .body = body_stmt } };

    // add(10, 20)
    const arg1 = try allocator.create(Ast.Expr);
    arg1.* = Ast.Expr{ .number = 10 };
    const arg2 = try allocator.create(Ast.Expr);
    arg2.* = Ast.Expr{ .number = 20 };

    const call_expr = try allocator.create(Ast.Expr);
    call_expr.* = Ast.Expr{ .call = .{ .callee = "add", .args = &[_]*Ast.Expr{ arg1, arg2 } } };

    const let_stmt = Ast.Stmt{ .let = .{ .name = "x", .value = call_expr } };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{ fn_decl, let_stmt } };

    var codegen = try Codegen.init(allocator);
    defer codegen.deinit();

    const code = try codegen.generate(program);
    defer allocator.free(code);

    // Check for function label
    try std.testing.expect(std.mem.indexOf(u8, code, "_add:") != null);
    // Check for call
    try std.testing.expect(std.mem.indexOf(u8, code, "call _add") != null);
    // Check for argument passing (pushing args)
    try std.testing.expect(std.mem.indexOf(u8, code, "push %rax") != null);
    // Check for argument popping into registers
    try std.testing.expect(std.mem.indexOf(u8, code, "pop %rsi") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "pop %rdi") != null);
}

test "codegen unary" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // let x = -10;
    // let y = !true;

    const ten = try allocator.create(Ast.Expr);
    ten.* = Ast.Expr{ .number = 10 };
    const neg_ten = try allocator.create(Ast.Expr);
    neg_ten.* = Ast.Expr{ .unary = .{ .op = .minus, .right = ten } };
    const let_x = Ast.Stmt{ .let = .{ .name = "x", .value = neg_ten } };

    const true_expr = try allocator.create(Ast.Expr);
    true_expr.* = Ast.Expr{ .boolean = true };
    const not_true = try allocator.create(Ast.Expr);
    not_true.* = Ast.Expr{ .unary = .{ .op = .bang, .right = true_expr } };
    const let_y = Ast.Stmt{ .let = .{ .name = "y", .value = not_true } };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{ let_x, let_y } };

    var codegen = try Codegen.init(allocator);
    defer codegen.deinit();

    const code = try codegen.generate(program);
    defer allocator.free(code);

    // Check for neg
    try std.testing.expect(std.mem.indexOf(u8, code, "neg %rax") != null);
    // Check for not (sete)
    try std.testing.expect(std.mem.indexOf(u8, code, "sete %al") != null);
}
