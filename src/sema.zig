const std = @import("std");
const Ast = @import("ast.zig");
const Diagnostics = @import("diagnostics.zig").Diagnostics;
const Token = @import("lexer.zig").Token;

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
    current_return_type: ?Ast.Type,
    diagnostics: *Diagnostics,
    loop_depth: usize,

    pub fn init(allocator: std.mem.Allocator, diagnostics: *Diagnostics) !Sema {
        const root = try allocator.create(Scope);
        root.* = Scope.init(allocator, null);
        return .{
            .allocator = allocator,
            .current_scope = root,
            .structs = std.StringHashMap(StructDef).init(allocator),
            .current_return_type = null,
            .diagnostics = diagnostics,
            .loop_depth = 0,
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
            self.analyzeStmt(stmt) catch |err| {
                if (err != error.SemanticError) return err;
            };
        }
    }

    fn analyzeStmt(self: *Sema, stmt: Ast.Stmt) !void {
        switch (stmt.data) {
            .let => |decl| {
                const type_info = try self.analyzeExpr(decl.value);
                if (decl.type) |explicit_type| {
                    if (!self.typesEqual(explicit_type, type_info)) {
                        try self.diagnostics.addError(stmt.loc, "Type mismatch in variable declaration. Expected {}, found {}", .{ explicit_type, type_info });
                        return error.SemanticError;
                    }
                }
                try self.current_scope.put(decl.name, type_info);
            },
            .assign => |assign| {
                const type_info = try self.analyzeExpr(assign.value);
                if (self.current_scope.get(assign.name)) |info| {
                    if (!self.typesEqual(info.type, type_info)) {
                        try self.diagnostics.addError(stmt.loc, "Type mismatch in assignment. Expected {}, found {}", .{ info.type, type_info });
                        return error.SemanticError;
                    }
                } else {
                    try self.reportUndefinedVariable(assign.name, stmt.loc);
                    return error.SemanticError;
                }
            },
            .array_assign => |aa| {
                const index_type = try self.analyzeExpr(aa.index);
                if (!self.typesEqual(index_type, .int)) {
                    try self.diagnostics.addError(aa.index.loc, "Array index must be an integer", .{});
                    return error.SemanticError;
                }
                const value_type = try self.analyzeExpr(aa.value);
                if (!self.typesEqual(value_type, .int)) {
                    try self.diagnostics.addError(aa.value.loc, "Array elements must be integers", .{});
                    return error.SemanticError;
                }

                if (self.current_scope.get(aa.name)) |info| {
                    if (!self.typesEqual(info.type, .array_int)) {
                        try self.diagnostics.addError(stmt.loc, "Variable '{s}' is not an array", .{aa.name});
                        return error.SemanticError;
                    }
                } else {
                    try self.reportUndefinedVariable(aa.name, stmt.loc);
                    return error.SemanticError;
                }
            },
            .struct_decl => |s| {
                var fields = std.StringHashMap(Ast.Type).init(self.allocator);
                const order = try self.allocator.alloc([]const u8, s.fields.len);
                for (s.fields, 0..) |field, i| {
                    try fields.put(field.name, field.type);
                    order[i] = field.name;
                }
                try self.structs.put(s.name, .{ .fields = fields, .field_order = order });
            },
            .print => |expr| {
                _ = try self.analyzeExpr(expr);
            },
            .block => |stmts| {
                try self.enterScope();
                for (stmts) |s| {
                    self.analyzeStmt(s) catch |err| {
                        if (err != error.SemanticError) return err;
                    };
                }
                self.leaveScope();
            },
            .if_stmt => |if_s| {
                const cond_type = try self.analyzeExpr(if_s.condition);
                if (!self.typesEqual(cond_type, .bool)) {
                    try self.diagnostics.addError(if_s.condition.loc, "If condition must be a boolean", .{});
                    return error.SemanticError;
                }
                try self.analyzeStmt(if_s.then_branch.*);
                if (if_s.else_branch) |else_branch| {
                    try self.analyzeStmt(else_branch.*);
                }
            },
            .while_stmt => |while_s| {
                const cond_type = try self.analyzeExpr(while_s.condition);
                if (!self.typesEqual(cond_type, .bool)) {
                    try self.diagnostics.addError(while_s.condition.loc, "While condition must be a boolean", .{});
                    return error.SemanticError;
                }
                self.loop_depth += 1;
                try self.analyzeStmt(while_s.body.*);
                self.loop_depth -= 1;
            },
            .fn_decl => |func| {
                try self.current_scope.put(func.name, .void);
                try self.enterScope();
                for (func.params) |param| {
                    try self.current_scope.put(param.name, param.type);
                }
                const prev_return_type = self.current_return_type;
                self.current_return_type = func.return_type;
                try self.analyzeStmt(func.body.*);
                self.current_return_type = prev_return_type;
                self.leaveScope();
            },
            .return_stmt => |ret| {
                if (ret) |expr| {
                    const type_info = try self.analyzeExpr(expr);
                    if (self.current_return_type) |expected| {
                        if (!self.typesEqual(expected, type_info)) {
                            try self.diagnostics.addError(stmt.loc, "Invalid return type. Expected {}, found {}", .{ expected, type_info });
                            return error.SemanticError;
                        }
                    }
                } else {
                    if (self.current_return_type) |expected| {
                        if (expected != .void) {
                            try self.diagnostics.addError(stmt.loc, "Expected return value of type {}", .{expected});
                            return error.SemanticError;
                        }
                    }
                }
            },
            .break_stmt, .continue_stmt => {
                if (self.loop_depth == 0) {
                    try self.diagnostics.addError(stmt.loc, "Break/Continue statement must be inside a loop", .{});
                    return error.SemanticError;
                }
            },
            .for_stmt => |for_s| {
                try self.enterScope();
                if (for_s.init) |init_s| {
                    try self.analyzeStmt(init_s.*);
                }
                if (for_s.condition) |cond| {
                    const cond_type = try self.analyzeExpr(cond);
                    if (!self.typesEqual(cond_type, .bool)) {
                        try self.diagnostics.addError(cond.loc, "For condition must be a boolean", .{});
                        return error.SemanticError;
                    }
                }
                if (for_s.increment) |incr| {
                    try self.analyzeStmt(incr.*);
                }
                self.loop_depth += 1;
                try self.analyzeStmt(for_s.body.*);
                self.loop_depth -= 1;
                self.leaveScope();
            },
        }
    }

    fn analyzeExpr(self: *Sema, expr: *Ast.Expr) !Ast.Type {
        switch (expr.data) {
            .number => return .int,
            .boolean => return .bool,
            .string => return .string,
            .identifier => |name| {
                if (self.current_scope.get(name)) |info| {
                    return info.type;
                } else {
                    try self.reportUndefinedVariable(name, expr.loc);
                    return error.SemanticError;
                }
            },
            .binary => |bin| {
                const left = try self.analyzeExpr(bin.left);
                const right = try self.analyzeExpr(bin.right);

                switch (bin.op) {
                    .add, .sub, .mul, .div => {
                        if (!self.typesEqual(left, .int) or !self.typesEqual(right, .int)) {
                            try self.diagnostics.addError(expr.loc, "Operands must be integers", .{});
                            return error.SemanticError;
                        }
                        return .int;
                    },
                    .equal_equal, .bang_equal => {
                        if (!self.typesEqual(left, right)) {
                            try self.diagnostics.addError(expr.loc, "Operands must be of the same type", .{});
                            return error.SemanticError;
                        }
                        return .bool;
                    },
                    .less, .less_equal, .greater, .greater_equal => {
                        if (!self.typesEqual(left, .int) or !self.typesEqual(right, .int)) {
                            try self.diagnostics.addError(expr.loc, "Operands must be integers", .{});
                            return error.SemanticError;
                        }
                        return .bool;
                    },
                    .logic_and, .logic_or => {
                        if (!self.typesEqual(left, .bool) or !self.typesEqual(right, .bool)) {
                            try self.diagnostics.addError(expr.loc, "Operands must be booleans", .{});
                            return error.SemanticError;
                        }
                        return .bool;
                    },
                }
            },
            .unary => |un| {
                const right = try self.analyzeExpr(un.right);
                switch (un.op) {
                    .bang => {
                        if (!self.typesEqual(right, .bool)) {
                            try self.diagnostics.addError(expr.loc, "Operand must be boolean", .{});
                            return error.SemanticError;
                        }
                        return .bool;
                    },
                    .minus => {
                        if (!self.typesEqual(right, .int)) {
                            try self.diagnostics.addError(expr.loc, "Operand must be integer", .{});
                            return error.SemanticError;
                        }
                        return .int;
                    },
                }
            },
            .call => |call| {
                if (!self.current_scope.contains(call.callee)) {
                    try self.reportUndefinedVariable(call.callee, expr.loc);
                    return error.SemanticError;
                }
                for (call.args) |arg| {
                    _ = try self.analyzeExpr(arg);
                }
                return .int;
            },
            .array_literal => |elements| {
                for (elements) |elem| {
                    const t = try self.analyzeExpr(elem);
                    if (!self.typesEqual(t, .int)) {
                        try self.diagnostics.addError(elem.loc, "Array elements must be integers", .{});
                        return error.SemanticError;
                    }
                }
                return .array_int;
            },
            .index => |idx| {
                const callee_type = try self.analyzeExpr(idx.callee);
                if (!self.typesEqual(callee_type, .array_int)) {
                    try self.diagnostics.addError(idx.callee.loc, "Expected array type", .{});
                    return error.SemanticError;
                }
                const index_type = try self.analyzeExpr(idx.index);
                if (!self.typesEqual(index_type, .int)) {
                    try self.diagnostics.addError(idx.index.loc, "Index must be an integer", .{});
                    return error.SemanticError;
                }
                return .int;
            },
            .struct_literal => |sl| {
                const def = self.structs.get(sl.struct_name) orelse {
                    try self.diagnostics.addError(expr.loc, "Undefined struct '{s}'", .{sl.struct_name});
                    return error.SemanticError;
                };
                for (sl.fields) |field| {
                    const field_type = def.fields.get(field.name) orelse {
                        try self.diagnostics.addError(expr.loc, "Struct '{s}' has no field '{s}'", .{ sl.struct_name, field.name });
                        return error.SemanticError;
                    };
                    const expr_type = try self.analyzeExpr(field.value);
                    if (!self.typesEqual(expr_type, field_type)) {
                        try self.diagnostics.addError(field.value.loc, "Type mismatch for field '{s}'", .{field.name});
                        return error.SemanticError;
                    }
                }
                return Ast.Type{ .struct_type = sl.struct_name };
            },
            .field_access => |fa| {
                const obj_type = try self.analyzeExpr(fa.object);
                switch (obj_type) {
                    .struct_type => |name| {
                        const def = self.structs.get(name) orelse {
                            // Should not happen if type exists
                            return error.SemanticError;
                        };
                        const field_type = def.fields.get(fa.field) orelse {
                            try self.diagnostics.addError(expr.loc, "Struct '{s}' has no field '{s}'", .{ name, fa.field });
                            return error.SemanticError;
                        };
                        return field_type;
                    },
                    else => {
                        try self.diagnostics.addError(fa.object.loc, "Expected struct type", .{});
                        return error.SemanticError;
                    },
                }
            },
        }
    }

    fn reportUndefinedVariable(self: *Sema, name: []const u8, loc: Token.Loc) !void {
        if (self.findClosestMatch(name)) |match| {
            try self.diagnostics.addErrorWithHint(loc, match, "undefined variable '{s}'", .{name});
        } else {
            try self.diagnostics.addError(loc, "undefined variable '{s}'", .{name});
        }
    }

    fn findClosestMatch(self: *Sema, name: []const u8) ?[]const u8 {
        var min_dist: usize = std.math.maxInt(usize);
        var best_match: ?[]const u8 = null;

        var scope: ?*Scope = self.current_scope;
        while (scope) |s| {
            var it = s.symbols.keyIterator();
            while (it.next()) |key| {
                const dist = levenshtein(name, key.*);
                if (dist < min_dist) {
                    min_dist = dist;
                    best_match = key.*;
                }
            }
            scope = s.parent;
        }

        if (min_dist <= 3 and min_dist < name.len) return best_match;
        return null;
    }

    fn levenshtein(a: []const u8, b: []const u8) usize {
        if (a.len == 0) return b.len;
        if (b.len == 0) return a.len;

        const allocator = std.heap.page_allocator; // Use page allocator for temp buffers
        const v0 = allocator.alloc(usize, b.len + 1) catch return std.math.maxInt(usize);
        defer allocator.free(v0);
        const v1 = allocator.alloc(usize, b.len + 1) catch return std.math.maxInt(usize);
        defer allocator.free(v1);

        for (v0, 0..) |*item, i| item.* = i;

        for (a, 0..) |char_a, i| {
            v1[0] = i + 1;
            for (b, 0..) |char_b, j| {
                const cost: usize = if (char_a == char_b) 0 else 1;
                v1[j + 1] = @min(v1[j] + 1, @min(v0[j + 1] + 1, v0[j] + cost));
            }
            @memcpy(v0, v1);
        }

        return v0[b.len];
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
    SemanticError,
};

test "sema basic" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    // let x = 10;
    const val = try allocator.create(Ast.Expr);
    val.* = Ast.Expr{ .loc = undefined, .data = .{ .number = 10 } };
    const stmt1 = Ast.Stmt{ .loc = undefined, .data = .{ .let = .{ .name = "x", .type = null, .value = val } } };

    // print(x);
    const ref = try allocator.create(Ast.Expr);
    ref.* = Ast.Expr{ .loc = undefined, .data = .{ .identifier = "x" } };
    const stmt2 = Ast.Stmt{ .loc = undefined, .data = .{ .print = ref } };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{ stmt1, stmt2 } };

    var sema = try Sema.init(allocator, &diagnostics);
    defer sema.deinit();

    try sema.analyze(program);
    try std.testing.expect(!diagnostics.hasErrors());
}

test "sema undefined variable" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    // print(y);
    const ref = try allocator.create(Ast.Expr);
    ref.* = Ast.Expr{ .loc = undefined, .data = .{ .identifier = "y" } };
    const stmt = Ast.Stmt{ .loc = undefined, .data = .{ .print = ref } };

    const program = Ast.Program{ .statements = &[_]Ast.Stmt{stmt} };

    var sema = try Sema.init(allocator, &diagnostics);
    defer sema.deinit();

    try sema.analyze(program);
    try std.testing.expect(diagnostics.hasErrors());
}
