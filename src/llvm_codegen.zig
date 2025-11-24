const std = @import("std");
const Ast = @import("ast.zig");

const SymbolInfo = struct {
    reg: []const u8, // The register name (e.g., "%x") or pointer to alloca
    type: Ast.Type,
};

const LoopLabels = struct {
    continue_label: usize,
    break_label: usize,
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

    pub fn put(self: *Scope, name: []const u8, reg: []const u8, type_info: Ast.Type) !void {
        try self.vars.put(name, .{ .reg = reg, .type = type_info });
    }

    pub fn get(self: *Scope, name: []const u8) ?SymbolInfo {
        if (self.vars.get(name)) |info| return info;
        if (self.parent) |p| return p.get(name);
        return null;
    }
};

const StructDef = struct {
    field_order: []const []const u8,
    field_types: []const Ast.Type,
};

pub const LLVMCodegen = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayListUnmanaged(u8),
    current_scope: *Scope,
    reg_counter: usize,
    label_counter: usize,
    string_literals: std.StringHashMap(usize),
    structs: std.StringHashMap(StructDef),
    loop_stack: std.ArrayListUnmanaged(LoopLabels),

    pub fn init(allocator: std.mem.Allocator) !LLVMCodegen {
        const root = try allocator.create(Scope);
        root.* = Scope.init(allocator, null);
        return .{
            .allocator = allocator,
            .output = .{},
            .current_scope = root,
            .reg_counter = 0,
            .label_counter = 0,
            .string_literals = std.StringHashMap(usize).init(allocator),
            .structs = std.StringHashMap(StructDef).init(allocator),
            .loop_stack = .{},
        };
    }

    pub fn deinit(self: *LLVMCodegen) void {
        self.output.deinit(self.allocator);
        self.string_literals.deinit();
        self.structs.deinit();
        self.loop_stack.deinit(self.allocator);
        var scope: ?*Scope = self.current_scope;
        while (scope) |s| {
            const parent = s.parent;
            s.deinit();
            self.allocator.destroy(s);
            scope = parent;
        }
    }

    fn enterScope(self: *LLVMCodegen) !void {
        const new_scope = try self.allocator.create(Scope);
        new_scope.* = Scope.init(self.allocator, self.current_scope);
        self.current_scope = new_scope;
    }

    fn leaveScope(self: *LLVMCodegen) void {
        if (self.current_scope.parent) |parent| {
            const old_scope = self.current_scope;
            old_scope.deinit();
            self.allocator.destroy(old_scope);
            self.current_scope = parent;
        }
    }

    fn newReg(self: *LLVMCodegen) ![]const u8 {
        const reg = try std.fmt.allocPrint(self.allocator, "%{d}", .{self.reg_counter});
        self.reg_counter += 1;
        return reg;
    }

    fn newLabel(self: *LLVMCodegen) usize {
        const label = self.label_counter;
        self.label_counter += 1;
        return label;
    }

    fn emit(self: *LLVMCodegen, comptime format: []const u8, args: anytype) !void {
        try self.output.writer(self.allocator).print(format, args);
    }

    fn getLLVMType(self: *LLVMCodegen, type_info: Ast.Type) ![]const u8 {
        switch (type_info) {
            .int => return "i64",
            .bool => return "i1",
            .string => return "i8*",
            .void => return "void",
            .array_int => return "i64*", // Simple pointer for now
            .struct_type => |name| return try std.fmt.allocPrint(self.allocator, "%struct.{s}*", .{name}),
        }
    }

    pub fn generate(self: *LLVMCodegen, program: Ast.Program) ![]const u8 {
        // Header
        try self.emit("; ModuleID = 'mini'\n", .{});
        try self.emit("source_filename = \"mini\"\n", .{});
        try self.emit("target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128\"\n", .{});
        try self.emit("target triple = \"x86_64-apple-macosx10.15.0\"\n\n", .{});

        // External declarations
        try self.emit("declare i32 @printf(i8*, ...)\n\n", .{});
        try self.emit("@.str_int = private unnamed_addr constant [5 x i8] c\"%ld\\0A\\00\", align 1\n", .{});
        try self.emit("@.str_str = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\", align 1\n", .{});

        // Process struct declarations first to define types
        for (program.statements) |stmt| {
            if (stmt.data == .struct_decl) {
                try self.genStructDecl(stmt.data.struct_decl);
            }
        }

        // Main function
        try self.emit("define i32 @main() {{\n", .{});
        try self.emit("entry:\n", .{});

        for (program.statements) |stmt| {
            if (stmt.data != .struct_decl and stmt.data != .fn_decl) {
                _ = try self.genStmt(stmt);
            }
        }

        try self.emit("  ret i32 0\n", .{});
        try self.emit("}}\n\n", .{});

        // Generate other functions
        for (program.statements) |stmt| {
            if (stmt.data == .fn_decl) {
                try self.genFnDecl(stmt.data.fn_decl);
            }
        }

        // String literals
        if (self.string_literals.count() > 0) {
            var it = self.string_literals.iterator();
            while (it.next()) |entry| {
                try self.emit("@.str.{d} = private unnamed_addr constant [{d} x i8] c\"{s}\\00\", align 1\n", .{ entry.value_ptr.*, entry.key_ptr.*.len + 1, entry.key_ptr.* });
            }
        }

        return self.output.toOwnedSlice(self.allocator);
    }

    fn genStructDecl(self: *LLVMCodegen, s: anytype) !void {
        try self.emit("%struct.{s} = type {{ ", .{s.name});
        const order = try self.allocator.alloc([]const u8, s.fields.len);
        const types = try self.allocator.alloc(Ast.Type, s.fields.len);
        for (s.fields, 0..) |field, i| {
            order[i] = field.name;
            types[i] = field.type;
            const llvm_type = try self.getLLVMType(field.type);
            try self.emit("{s}", .{llvm_type});
            if (i < s.fields.len - 1) {
                try self.emit(", ", .{});
            }
        }
        try self.emit(" }}\n\n", .{});
        try self.structs.put(s.name, .{ .field_order = order, .field_types = types });
    }

    fn genFnDecl(self: *LLVMCodegen, func: anytype) !void {
        const ret_type = try self.getLLVMType(func.return_type);
        try self.emit("define {s} @{s}(", .{ ret_type, func.name });

        for (func.params, 0..) |param, i| {
            const param_type = try self.getLLVMType(param.type);
            try self.emit("{s} %{s}", .{ param_type, param.name });
            if (i < func.params.len - 1) {
                try self.emit(", ", .{});
            }
        }
        try self.emit(") {{\n", .{});
        try self.emit("entry:\n", .{});

        // Reset register counter for new function
        const old_reg_counter = self.reg_counter;
        self.reg_counter = 0; // Arguments are named, so we can start from 0 or just use names
        // Actually, LLVM allows named registers. But to avoid collision with our %0, %1...
        // Let's just allocate stack space for arguments so we can treat them like local variables

        try self.enterScope();

        for (func.params) |param| {
            const param_type = try self.getLLVMType(param.type);
            const ptr = try self.newReg();
            try self.emit("  {s} = alloca {s}, align 8\n", .{ ptr, param_type });
            try self.emit("  store {s} %{s}, {s}* {s}, align 8\n", .{ param_type, param.name, param_type, ptr });
            try self.current_scope.put(param.name, ptr, param.type);
        }

        const terminated = try self.genStmt(func.body.*);

        if (!terminated) {
            if (func.return_type == .void) {
                try self.emit("  ret void\n", .{});
            } else if (func.return_type == .int) {
                try self.emit("  ret i64 0\n", .{});
            }
        }

        try self.emit("}}\n\n", .{});
        self.leaveScope();
        self.reg_counter = old_reg_counter;
    }

    fn genStmt(self: *LLVMCodegen, stmt: Ast.Stmt) !bool {
        switch (stmt.data) {
            .let => |decl| {
                const val_reg = try self.genExpr(decl.value);
                const type_info = try self.getExprType(decl.value);
                const llvm_type = try self.getLLVMType(type_info);

                const ptr = try self.newReg();
                try self.emit("  {s} = alloca {s}, align 8\n", .{ ptr, llvm_type });
                try self.emit("  store {s} {s}, {s}* {s}, align 8\n", .{ llvm_type, val_reg, llvm_type, ptr });

                try self.current_scope.put(decl.name, ptr, type_info);
                return false;
            },
            .assign => |assign| {
                const val_reg = try self.genExpr(assign.value);
                const info = self.current_scope.get(assign.name) orelse return error.UnknownVariable;
                const llvm_type = try self.getLLVMType(info.type);
                try self.emit("  store {s} {s}, {s}* {s}, align 8\n", .{ llvm_type, val_reg, llvm_type, info.reg });
                return false;
            },
            .print => |expr| {
                const val_reg = try self.genExpr(expr);
                const type_info = try self.getExprType(expr);

                if (type_info == .int) {
                    const call_reg = try self.newReg();
                    try self.emit("  {s} = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str_int, i64 0, i64 0), i64 {s})\n", .{ call_reg, val_reg });
                } else if (type_info == .string) {
                    const call_reg = try self.newReg();
                    try self.emit("  {s} = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str_str, i64 0, i64 0), i8* {s})\n", .{ call_reg, val_reg });
                } else if (type_info == .bool) {
                    // Print 1 or 0
                    const zext_reg = try self.newReg();
                    try self.emit("  {s} = zext i1 {s} to i64\n", .{ zext_reg, val_reg });
                    const call_reg = try self.newReg();
                    try self.emit("  {s} = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str_int, i64 0, i64 0), i64 {s})\n", .{ call_reg, zext_reg });
                }
                return false;
            },
            .block => |stmts| {
                try self.enterScope();
                var terminated = false;
                for (stmts) |s| {
                    if (try self.genStmt(s)) {
                        terminated = true;
                    }
                }
                self.leaveScope();
                return terminated;
            },
            .if_stmt => |if_s| {
                const cond_reg = try self.genExpr(if_s.condition);
                const then_label = self.newLabel();
                const end_label = self.newLabel();

                if (if_s.else_branch) |else_branch| {
                    const else_label = self.newLabel();
                    try self.emit("  br i1 {s}, label %L{d}, label %L{d}\n", .{ cond_reg, then_label, else_label });

                    try self.emit("L{d}:\n", .{then_label});
                    const then_terminated = try self.genStmt(if_s.then_branch.*);
                    if (!then_terminated) {
                        try self.emit("  br label %L{d}\n", .{end_label});
                    }

                    try self.emit("L{d}:\n", .{else_label});
                    const else_terminated = try self.genStmt(else_branch.*);
                    if (!else_terminated) {
                        try self.emit("  br label %L{d}\n", .{end_label});
                    }
                } else {
                    try self.emit("  br i1 {s}, label %L{d}, label %L{d}\n", .{ cond_reg, then_label, end_label });

                    try self.emit("L{d}:\n", .{then_label});
                    const then_terminated = try self.genStmt(if_s.then_branch.*);
                    if (!then_terminated) {
                        try self.emit("  br label %L{d}\n", .{end_label});
                    }
                }

                try self.emit("L{d}:\n", .{end_label});
                return false;
            },
            .while_stmt => |while_s| {
                const start_label = self.newLabel();
                const body_label = self.newLabel();
                const end_label = self.newLabel();

                try self.loop_stack.append(self.allocator, .{ .continue_label = start_label, .break_label = end_label });

                try self.emit("  br label %L{d}\n", .{start_label});
                try self.emit("L{d}:\n", .{start_label});

                const cond_reg = try self.genExpr(while_s.condition);
                try self.emit("  br i1 {s}, label %L{d}, label %L{d}\n", .{ cond_reg, body_label, end_label });

                try self.emit("L{d}:\n", .{body_label});
                const body_terminated = try self.genStmt(while_s.body.*);
                if (!body_terminated) {
                    try self.emit("  br label %L{d}\n", .{start_label});
                }

                try self.emit("L{d}:\n", .{end_label});
                _ = self.loop_stack.pop();
                return false;
            },
            .for_stmt => |for_s| {
                try self.enterScope();
                if (for_s.init) |init_s| {
                    _ = try self.genStmt(init_s.*);
                }

                const start_label = self.newLabel();
                const body_label = self.newLabel();
                const incr_label = self.newLabel();
                const end_label = self.newLabel();

                try self.loop_stack.append(self.allocator, .{ .continue_label = incr_label, .break_label = end_label });

                try self.emit("  br label %L{d}\n", .{start_label});
                try self.emit("L{d}:\n", .{start_label});

                if (for_s.condition) |cond| {
                    const cond_reg = try self.genExpr(cond);
                    try self.emit("  br i1 {s}, label %L{d}, label %L{d}\n", .{ cond_reg, body_label, end_label });
                } else {
                    try self.emit("  br label %L{d}\n", .{body_label});
                }

                try self.emit("L{d}:\n", .{body_label});
                const body_terminated = try self.genStmt(for_s.body.*);
                if (!body_terminated) {
                    try self.emit("  br label %L{d}\n", .{incr_label});
                }

                try self.emit("L{d}:\n", .{incr_label});
                if (for_s.increment) |incr| {
                    _ = try self.genStmt(incr.*);
                }
                try self.emit("  br label %L{d}\n", .{start_label});

                try self.emit("L{d}:\n", .{end_label});
                _ = self.loop_stack.pop();
                self.leaveScope();
                return false;
            },
            .break_stmt => {
                if (self.loop_stack.items.len == 0) return error.InvalidJump;
                const labels = self.loop_stack.items[self.loop_stack.items.len - 1];
                try self.emit("  br label %L{d}\n", .{labels.break_label});
                return true;
            },
            .continue_stmt => {
                if (self.loop_stack.items.len == 0) return error.InvalidJump;
                const labels = self.loop_stack.items[self.loop_stack.items.len - 1];
                try self.emit("  br label %L{d}\n", .{labels.continue_label});
                return true;
            },
            .return_stmt => |ret| {
                if (ret) |expr| {
                    const val_reg = try self.genExpr(expr);
                    const type_info = try self.getExprType(expr);
                    const llvm_type = try self.getLLVMType(type_info);
                    try self.emit("  ret {s} {s}\n", .{ llvm_type, val_reg });
                } else {
                    try self.emit("  ret void\n", .{});
                }
                return true;
            },
            .array_assign => |aa| {
                const info = self.current_scope.get(aa.name) orelse return error.UnknownVariable;
                const idx_reg = try self.genExpr(aa.index);
                const val_reg = try self.genExpr(aa.value);

                const arr_ptr_reg = try self.newReg();
                try self.emit("  {s} = load i64*, i64** {s}, align 8\n", .{ arr_ptr_reg, info.reg });

                const elem_ptr = try self.newReg();
                try self.emit("  {s} = getelementptr inbounds i64, i64* {s}, i64 {s}\n", .{ elem_ptr, arr_ptr_reg, idx_reg });

                try self.emit("  store i64 {s}, i64* {s}, align 8\n", .{ val_reg, elem_ptr });
                return false;
            },
            else => return false,
        }
    }

    fn genExpr(self: *LLVMCodegen, expr: *Ast.Expr) ![]const u8 {
        switch (expr.data) {
            .number => |val| {
                return try std.fmt.allocPrint(self.allocator, "{d}", .{val});
            },
            .boolean => |val| {
                return if (val) "1" else "0";
            },
            .string => |s| {
                const label = if (self.string_literals.get(s)) |l| l else blk: {
                    const l = self.newLabel();
                    try self.string_literals.put(s, l);
                    break :blk l;
                };
                const reg = try self.newReg();
                try self.emit("  {s} = getelementptr inbounds [{d} x i8], [{d} x i8]* @.str.{d}, i64 0, i64 0\n", .{ reg, s.len + 1, s.len + 1, label });
                return reg;
            },
            .identifier => |name| {
                const info = self.current_scope.get(name) orelse return error.UnknownVariable;
                const reg = try self.newReg();
                const llvm_type = try self.getLLVMType(info.type);
                try self.emit("  {s} = load {s}, {s}* {s}, align 8\n", .{ reg, llvm_type, llvm_type, info.reg });
                return reg;
            },
            .binary => |bin| {
                const left_reg = try self.genExpr(bin.left);
                const right_reg = try self.genExpr(bin.right);
                const res_reg = try self.newReg();

                switch (bin.op) {
                    .add => try self.emit("  {s} = add i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .sub => try self.emit("  {s} = sub i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .mul => try self.emit("  {s} = mul i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .div => try self.emit("  {s} = sdiv i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .equal_equal => try self.emit("  {s} = icmp eq i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }), // Assuming int for now
                    .bang_equal => try self.emit("  {s} = icmp ne i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .less => try self.emit("  {s} = icmp slt i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .less_equal => try self.emit("  {s} = icmp sle i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .greater => try self.emit("  {s} = icmp sgt i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .greater_equal => try self.emit("  {s} = icmp sge i64 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .logic_and => try self.emit("  {s} = and i1 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                    .logic_or => try self.emit("  {s} = or i1 {s}, {s}\n", .{ res_reg, left_reg, right_reg }),
                }
                return res_reg;
            },
            .unary => |un| {
                const right_reg = try self.genExpr(un.right);
                const res_reg = try self.newReg();
                switch (un.op) {
                    .minus => try self.emit("  {s} = sub i64 0, {s}\n", .{ res_reg, right_reg }),
                    .bang => try self.emit("  {s} = xor i1 {s}, 1\n", .{ res_reg, right_reg }),
                }
                return res_reg;
            },
            .call => |call| {
                // Generate arguments first
                const arg_regs = try self.allocator.alloc([]const u8, call.args.len);
                const arg_types = try self.allocator.alloc(Ast.Type, call.args.len);

                for (call.args, 0..) |arg, i| {
                    arg_regs[i] = try self.genExpr(arg);
                    arg_types[i] = try self.getExprType(arg);
                }

                const res_reg = try self.newReg();
                try self.emit("  {s} = call i64 @{s}(", .{ res_reg, call.callee });

                for (arg_regs, 0..) |reg, i| {
                    const llvm_type = try self.getLLVMType(arg_types[i]);
                    try self.emit("{s} {s}", .{ llvm_type, reg });
                    if (i < call.args.len - 1) {
                        try self.emit(", ", .{});
                    }
                }
                try self.emit(")\n", .{});
                return res_reg;
            },
            .array_literal => |items| {
                const arr_ptr = try self.newReg();
                try self.emit("  {s} = alloca [{d} x i64], align 8\n", .{ arr_ptr, items.len });

                for (items, 0..) |item, i| {
                    const val_reg = try self.genExpr(item);
                    const elem_ptr = try self.newReg();
                    try self.emit("  {s} = getelementptr inbounds [{d} x i64], [{d} x i64]* {s}, i64 0, i64 {d}\n", .{ elem_ptr, items.len, items.len, arr_ptr, i });
                    try self.emit("  store i64 {s}, i64* {s}, align 8\n", .{ val_reg, elem_ptr });
                }

                const ptr_reg = try self.newReg();
                try self.emit("  {s} = getelementptr inbounds [{d} x i64], [{d} x i64]* {s}, i64 0, i64 0\n", .{ ptr_reg, items.len, items.len, arr_ptr });
                return ptr_reg;
            },
            .index => |idx| {
                const target_reg = try self.genExpr(idx.callee);
                const index_reg = try self.genExpr(idx.index);

                const elem_ptr = try self.newReg();
                try self.emit("  {s} = getelementptr inbounds i64, i64* {s}, i64 {s}\n", .{ elem_ptr, target_reg, index_reg });

                const res_reg = try self.newReg();
                try self.emit("  {s} = load i64, i64* {s}, align 8\n", .{ res_reg, elem_ptr });
                return res_reg;
            },
            .struct_literal => |sl| {
                const struct_name = sl.struct_name;
                const ptr = try self.newReg();
                try self.emit("  {s} = alloca %struct.{s}, align 8\n", .{ ptr, struct_name });

                const def = self.structs.get(struct_name) orelse return error.UnknownStruct;

                for (sl.fields) |field| {
                    var field_idx: usize = 0;
                    var found = false;
                    for (def.field_order, 0..) |fname, i| {
                        if (std.mem.eql(u8, fname, field.name)) {
                            field_idx = i;
                            found = true;
                            break;
                        }
                    }
                    if (!found) return error.UnknownField;

                    const val_reg = try self.genExpr(field.value);
                    const field_ptr = try self.newReg();
                    try self.emit("  {s} = getelementptr inbounds %struct.{s}, %struct.{s}* {s}, i32 0, i32 {d}\n", .{ field_ptr, struct_name, struct_name, ptr, field_idx });

                    const field_type = def.field_types[field_idx];
                    const llvm_type = try self.getLLVMType(field_type);
                    try self.emit("  store {s} {s}, {s}* {s}, align 8\n", .{ llvm_type, val_reg, llvm_type, field_ptr });
                }

                return ptr;
            },
            .field_access => |fa| {
                const obj_reg = try self.genExpr(fa.object);
                const obj_type = try self.getExprType(fa.object);

                const struct_name = switch (obj_type) {
                    .struct_type => |name| name,
                    else => return error.NotStruct,
                };

                const def = self.structs.get(struct_name) orelse return error.UnknownStruct;

                var field_idx: usize = 0;
                var found = false;
                var field_type: Ast.Type = .int;

                for (def.field_order, 0..) |fname, i| {
                    if (std.mem.eql(u8, fname, fa.field)) {
                        field_idx = i;
                        field_type = def.field_types[i];
                        found = true;
                        break;
                    }
                }
                if (!found) return error.UnknownField;

                const field_ptr = try self.newReg();
                try self.emit("  {s} = getelementptr inbounds %struct.{s}, %struct.{s}* {s}, i32 0, i32 {d}\n", .{ field_ptr, struct_name, struct_name, obj_reg, field_idx });

                const res_reg = try self.newReg();
                const llvm_type = try self.getLLVMType(field_type);
                try self.emit("  {s} = load {s}, {s}* {s}, align 8\n", .{ res_reg, llvm_type, llvm_type, field_ptr });

                return res_reg;
            },
        }
    }

    fn getExprType(self: *LLVMCodegen, expr: *Ast.Expr) !Ast.Type {
        switch (expr.data) {
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
            .struct_literal => |sl| return Ast.Type{ .struct_type = sl.struct_name },
            .field_access => |fa| {
                const obj_type = try self.getExprType(fa.object);
                const struct_name = switch (obj_type) {
                    .struct_type => |name| name,
                    else => return error.NotStruct,
                };
                const def = self.structs.get(struct_name) orelse return error.UnknownStruct;
                for (def.field_order, 0..) |fname, i| {
                    if (std.mem.eql(u8, fname, fa.field)) {
                        return def.field_types[i];
                    }
                }
                return error.UnknownField;
            },
        }
    }
};
