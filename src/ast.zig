const std = @import("std");

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

pub const Expr = union(enum) {
    number: i64,
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
};

pub const Stmt = union(enum) {
    let: struct {
        name: []const u8,
        value: *Expr,
    },
    assign: struct {
        name: []const u8,
        value: *Expr,
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
    fn_decl: struct {
        name: []const u8,
        params: []const []const u8,
        body: *Stmt,
    },
    return_stmt: ?*Expr,
};

pub const Type = enum {
    int,
    string,
    bool,
    void,
};

pub const Program = struct {
    statements: []const Stmt,

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        // In a real compiler, we'd likely use an ArenaAllocator for the whole AST
        // so we wouldn't need manual deinit for every node.
        // For this simple example, we'll assume the caller handles memory
        // or we use an Arena.
        _ = self;
        _ = allocator;
    }
};
