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
            .array_literal => return .array_int,
            .index => return .int,
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
            .array_assign => |aa| {
                const info = self.current_scope.get(aa.name) orelse return error.UnknownVariable;

                // Calculate address
                try self.genExpr(aa.index);
                try self.emit("  push %rax\n", .{}); // Push index

                try self.genExpr(aa.value);
                try self.emit("  push %rax\n", .{}); // Push value

                // Load base pointer
                try self.emit("  mov -{d}(%rbp), %rbx\n", .{info.offset});

                // Pop value
                try self.emit("  pop %rcx\n", .{}); // Value in rcx

                // Pop index
                try self.emit("  pop %rax\n", .{}); // Index in rax

                // Calculate address: Base - Index * 8
                try self.emit("  imul $8, %rax\n", .{});
                try self.emit("  sub %rax, %rbx\n", .{});

                // Store value
                try self.emit("  mov %rcx, (%rbx)\n", .{});
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
            .for_stmt => |for_s| {
                try self.enterScope();

                if (for_s.init) |init_s| {
                    try self.genStmt(init_s.*);
                }

                const start_label = self.newLabel();
                const end_label = self.newLabel();

                try self.emit("L{d}:\n", .{start_label});

                if (for_s.condition) |cond| {
                    try self.genExpr(cond);
                    try self.emit("  cmp $0, %rax\n", .{});
                    try self.emit("  je L{d}\n", .{end_label});
                }

                try self.genStmt(for_s.body.*);

                if (for_s.increment) |incr| {
                    try self.genStmt(incr.*);
                }

                try self.emit("  jmp L{d}\n", .{start_label});
                try self.emit("L{d}:\n", .{end_label});

                self.leaveScope();
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
            .array_literal => |elements| {
                const start_offset = self.stack_offset;

                for (elements) |elem| {
                    try self.genExpr(elem);
                    self.stack_offset += 8;
                    try self.emit("  mov %rax, -{d}(%rbp)\n", .{self.stack_offset});
                }

                // Return address of first element
                try self.emit("  lea -{d}(%rbp), %rax\n", .{start_offset + 8});
            },
            .index => |idx| {
                try self.genExpr(idx.callee); // Returns pointer to base (address of [0])
                try self.emit("  push %rax\n", .{});

                try self.genExpr(idx.index);
                try self.emit("  mov %rax, %rbx\n", .{}); // Index in rbx

                try self.emit("  pop %rax\n", .{}); // Base in rax

                // Address = Base - Index * 8
                try self.emit("  imul $8, %rbx\n", .{});
                try self.emit("  sub %rbx, %rax\n", .{});

                // Load value
                try self.emit("  mov (%rax), %rax\n", .{});
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

test "codegen for loop" {
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

    var codegen = try Codegen.init(allocator);
    defer codegen.deinit();

    const code = try codegen.generate(program);
    defer allocator.free(code);

    // Check for loop structure
    // We expect a jump back to start
    // We expect a conditional jump to end
    // We expect init code
    // We expect increment code

    // Since labels are dynamic (L0, L1...), we can't check exact strings easily for labels.
    // But we can check for instructions.

    try std.testing.expect(std.mem.indexOf(u8, code, "jmp L") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "je L") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "add %rbx, %rax") != null); // i + 1
}

test "codegen arrays" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // let arr = [1, 2, 3];
    const one = try allocator.create(Ast.Expr);
    one.* = Ast.Expr{ .number = 1 };
    const two = try allocator.create(Ast.Expr);
    two.* = Ast.Expr{ .number = 2 };
    const three = try allocator.create(Ast.Expr);
    three.* = Ast.Expr{ .number = 3 };

    const array_lit = try allocator.create(Ast.Expr);
    array_lit.* = Ast.Expr{ .array_literal = &[_]*Ast.Expr{ one, two, three } };

    const let_arr = Ast.Stmt{ .let = .{ .name = "arr", .value = array_lit } };

    // let x = arr[1];
    const arr_ref = try allocator.create(Ast.Expr);
    arr_ref.* = Ast.Expr{ .identifier = "arr" };
    const idx_1 = try allocator.create(Ast.Expr);
    idx_1.* = Ast.Expr{ .number = 1 };

    const index_expr = try allocator.create(Ast.Expr);
    index_expr.* = Ast.Expr{ .index = .{ .callee = arr_ref, .index = idx_1 } };

    const let_x = Ast.Stmt{ .let = .{ .name = "x", .value = index_expr } };

    // arr[2] = 42;
    const arr_ref_2 = try allocator.create(Ast.Expr);
    arr_ref_2.* = Ast.Expr{ .identifier = "arr" };
    const idx_2 = try allocator.create(Ast.Expr);
    idx_2.* = Ast.Expr{ .number = 2 };
    const val_42 = try allocator.create(Ast.Expr);
    val_42.* = Ast.Expr{ .number = 42 };

    const assign_stmt = Ast.Stmt{ .array_assign = .{ .name = "arr", .index = idx_2, .value = val_42 } };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{ let_arr, let_x, assign_stmt } };

    var codegen = try Codegen.init(allocator);
    defer codegen.deinit();

    const code = try codegen.generate(program);
    defer allocator.free(code);

    // Check for array literal construction (stack stores)
    // We expect 3 stores for 1, 2, 3
    try std.testing.expect(std.mem.indexOf(u8, code, "mov $1, %rax") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "mov $2, %rax") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "mov $3, %rax") != null);

    // Check for address calculation (imul $8)
    try std.testing.expect(std.mem.indexOf(u8, code, "imul $8") != null);

    // Check for load (mov (%rax), %rax)
    try std.testing.expect(std.mem.indexOf(u8, code, "mov (%rax), %rax") != null);

    // Check for store (mov %rcx, (%rbx)) - registers might vary but pattern is store to memory
    // In array_assign: mov %rcx, (%rbx)
    try std.testing.expect(std.mem.indexOf(u8, code, "mov %rcx, (%rbx)") != null);
}
