const std = @import("std");
const Token = @import("lexer.zig").Token;

pub const BinaryOp = enum {
    add,
    sub,
    mul,
    div,
    equal_equal,
    bang_equal,
    less,
    less_equal,
    greater,
    greater_equal,
    logic_and,
    logic_or,
};

pub const UnaryOp = enum {
    bang,
    minus,
};

pub const Expr = struct {
    loc: Token.Loc,
    data: ExprData,
};

pub const ExprData = union(enum) {
    number: i64,
    float: f64,
    boolean: bool,
    string: []const u8,
    identifier: []const u8,
    unary: struct {
        op: UnaryOp,
        right: *Expr,
    },
    binary: struct {
        left: *Expr,
        op: BinaryOp,
        right: *Expr,
    },
    call: struct {
        callee: []const u8,
        args: []const *Expr,
    },
    array_literal: []const *Expr,
    index: struct {
        callee: *Expr,
        index: *Expr,
    },
    struct_literal: struct {
        struct_name: []const u8,
        fields: []const StructFieldInit,
    },
    field_access: struct {
        object: *Expr,
        field: []const u8,
    },
};

pub const StructFieldInit = struct {
    name: []const u8,
    value: *Expr,
};

pub const Type = union(enum) {
    int,
    float,
    string,
    bool,
    void,
    array_int,
    struct_type: []const u8,
};

pub const StructField = struct {
    name: []const u8,
    type: Type,
};

pub const FnParam = struct {
    name: []const u8,
    type: Type,
};

pub const Stmt = struct {
    loc: Token.Loc,
    data: StmtData,
};

pub const StmtData = union(enum) {
    let: struct {
        name: []const u8,
        type: ?Type,
        value: *Expr,
    },
    assign: struct {
        name: []const u8,
        value: *Expr,
    },
    array_assign: struct {
        name: []const u8,
        index: *Expr,
        value: *Expr,
    },
    struct_decl: struct {
        name: []const u8,
        fields: []const StructField,
    },
    print: *Expr,
    block: []const Stmt,
    if_stmt: struct {
        condition: *Expr,
        then_branch: *Stmt,
        else_branch: ?*Stmt,
    },
    while_stmt: struct {
        condition: *Expr,
        body: *Stmt,
    },
    for_stmt: struct {
        init: ?*Stmt,
        condition: ?*Expr,
        increment: ?*Stmt,
        body: *Stmt,
    },
    fn_decl: struct {
        name: []const u8,
        params: []const FnParam,
        return_type: Type,
        body: *Stmt,
    },
    return_stmt: ?*Expr,
    break_stmt: void,
    continue_stmt: void,
};

pub const Program = struct {
    statements: []const Stmt,

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }
};
