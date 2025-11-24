const std = @import("std");
const builtin = @import("builtin");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Sema = @import("sema.zig").Sema;
const LLVMCodegen = @import("llvm_codegen.zig").LLVMCodegen;
const Diagnostics = @import("diagnostics.zig").Diagnostics;

const runtime_c =
    \\#include <stdio.h>
    \\
    \\void print_int(long x) {
    \\    printf("%ld\n", x);
    \\}
    \\
    \\void print_string(char* s) {
    \\    printf("%s\n", s);
    \\}
;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    // No need to free args explicitly as arena handles it

    if (args.len < 2) {
        std.debug.print("Usage: {s} <file>\n", .{args[0]});
        return;
    }

    const file_path = args[1];
    if (!std.mem.endsWith(u8, file_path, ".mini")) {
        std.debug.print("Error: Source file must have .mini extension\n", .{});
        return;
    }

    const source = try std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024);

    // Parse
    var parser = Parser.init(allocator, source);
    const program = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return;
    };

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);
    // defer diagnostics.deinit(); // Arena handles it

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    // defer sema.deinit(); // Arena handles it
    sema.analyze(program) catch |err| {
        std.debug.print("Semantic error: {}\n", .{err});
        return;
    };

    if (diagnostics.hasErrors()) {
        diagnostics.report(source);
        return;
    }

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    // defer codegen.deinit(); // Arena handles it
    const code = codegen.generate(program) catch |err| {
        std.debug.print("Codegen error: {}\n", .{err});
        return;
    };

    // Prepare output directory
    const out_dir_name = "zig-out/tmp";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "program";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    // Handle macOS ARM -> x86_64 cross-compilation (since our codegen is x86_64)
    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    if (run_result.term.Exited != 0) {
        std.debug.print("Runtime error (exit code {}):\n", .{run_result.term.Exited});
    }

    const stdout = std.fs.File.stdout();
    try stdout.writeAll(run_result.stdout);
    try stdout.writeAll(run_result.stderr);
}

test {
    _ = Lexer;
    _ = Parser;
    _ = Sema;
    _ = LLVMCodegen;
}

test "integration control flow" {
    if (builtin.os.tag != .macos and builtin.os.tag != .linux) return error.SkipZigTest;

    const source =
        \\let x = 0;
        \\let sum = 0;
        \\while (x < 5) {
        \\    sum = sum + x;
        \\    x = x + 1;
        \\}
        \\print(sum);
        \\
        \\if (sum == 10) {
        \\    print(1);
        \\} else {
        \\    print(0);
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse
    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    try sema.analyze(program);

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    const code = try codegen.generate(program);

    // Prepare output directory
    const out_dir_name = "zig-out/test_tmp";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "test_program";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return error.CompilationFailed;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    try std.testing.expectEqual(run_result.term.Exited, 0);
    try std.testing.expectEqualStrings("10\n1\n", run_result.stdout);
}

test "integration functions" {
    if (builtin.os.tag != .macos and builtin.os.tag != .linux) return error.SkipZigTest;

    const source =
        \\fn add(a: int, b: int) -> int {
        \\    return a + b;
        \\}
        \\
        \\fn fib(n: int) -> int {
        \\    if (n < 2) {
        \\        return n;
        \\    }
        \\    return fib(n - 1) + fib(n - 2);
        \\}
        \\
        \\let x = add(10, 20);
        \\print(x);
        \\
        \\let f = fib(6);
        \\print(f);
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse
    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    try sema.analyze(program);

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    const code = try codegen.generate(program);

    // Prepare output directory
    const out_dir_name = "zig-out/test_tmp_func";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "test_program_func";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return error.CompilationFailed;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    try std.testing.expectEqual(run_result.term.Exited, 0);
    // add(10, 20) = 30
    // fib(6) = 8 (0, 1, 1, 2, 3, 5, 8)
    try std.testing.expectEqualStrings("30\n8\n", run_result.stdout);
}

test "integration strings" {
    if (builtin.os.tag != .macos and builtin.os.tag != .linux) return error.SkipZigTest;

    const source =
        \\let s = "hello world";
        \\print(s);
        \\print("direct string");
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse
    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    try sema.analyze(program);

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    const code = try codegen.generate(program);

    // Prepare output directory
    const out_dir_name = "zig-out/test_tmp_str";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "test_program_str";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return error.CompilationFailed;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    try std.testing.expectEqual(run_result.term.Exited, 0);
    try std.testing.expectEqualStrings("hello world\ndirect string\n", run_result.stdout);
}

test "integration booleans" {
    if (builtin.os.tag != .macos and builtin.os.tag != .linux) return error.SkipZigTest;

    const source =
        \\let t = true;
        \\let f = false;
        \\print(t);
        \\print(f);
        \\
        \\if (t && t) { print(1); } else { print(0); }
        \\if (t && f) { print(1); } else { print(0); }
        \\if (f && t) { print(1); } else { print(0); }
        \\if (f && f) { print(1); } else { print(0); }
        \\
        \\if (t || t) { print(1); } else { print(0); }
        \\if (t || f) { print(1); } else { print(0); }
        \\if (f || t) { print(1); } else { print(0); }
        \\if (f || f) { print(1); } else { print(0); }
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse
    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    try sema.analyze(program);

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    const code = try codegen.generate(program);

    // Prepare output directory
    const out_dir_name = "zig-out/test_tmp_bool";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "test_program_bool";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return error.CompilationFailed;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    try std.testing.expectEqual(run_result.term.Exited, 0);
    try std.testing.expectEqualStrings("1\n0\n1\n0\n0\n0\n1\n1\n1\n0\n", run_result.stdout);
}

test "integration unary" {
    if (builtin.os.tag != .macos and builtin.os.tag != .linux) return error.SkipZigTest;

    const source =
        \\let x = 10;
        \\let y = -x;
        \\print(y);
        \\
        \\let t = true;
        \\let f = !t;
        \\print(f);
        \\
        \\if (!f) { print(1); } else { print(0); }
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse
    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    try sema.analyze(program);

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    const code = try codegen.generate(program);

    // Prepare output directory
    const out_dir_name = "zig-out/test_tmp_unary";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "test_program_unary";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return error.CompilationFailed;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    try std.testing.expectEqual(run_result.term.Exited, 0);
    // y = -10
    // f = false (0)
    // !f = true (1)
    try std.testing.expectEqualStrings("-10\n0\n1\n", run_result.stdout);
}

test "integration for loop" {
    if (builtin.os.tag != .macos and builtin.os.tag != .linux) return error.SkipZigTest;

    const source =
        \\let sum = 0;
        \\for (let i = 0; i < 5; i = i + 1) {
        \\    sum = sum + i;
        \\}
        \\print(sum);
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse
    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    try sema.analyze(program);

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    const code = try codegen.generate(program);

    // Prepare output directory
    const out_dir_name = "zig-out/test_tmp_for";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "test_program_for";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return error.CompilationFailed;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    try std.testing.expectEqual(run_result.term.Exited, 0);
    // 0 + 1 + 2 + 3 + 4 = 10
    try std.testing.expectEqualStrings("10\n", run_result.stdout);
}

test "integration arrays" {
    if (builtin.os.tag != .macos and builtin.os.tag != .linux) return error.SkipZigTest;

    const source =
        \\let arr = [10, 20, 30];
        \\print(arr[0]);
        \\print(arr[1]);
        \\print(arr[2]);
        \\
        \\arr[1] = 42;
        \\print(arr[1]);
        \\
        \\let sum = 0;
        \\for (let i = 0; i < 3; i = i + 1) {
        \\    sum = sum + arr[i];
        \\}
        \\print(sum);
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse
    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    try sema.analyze(program);

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    const code = try codegen.generate(program);

    // Prepare output directory
    const out_dir_name = "zig-out/test_tmp_arrays";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "test_program_arrays";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return error.CompilationFailed;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    try std.testing.expectEqual(run_result.term.Exited, 0);
    // 10, 20, 30, 42, 82
    try std.testing.expectEqualStrings("10\n20\n30\n42\n82\n", run_result.stdout);
}

test "integration structs" {
    if (builtin.os.tag != .macos and builtin.os.tag != .linux) return error.SkipZigTest;

    const source =
        \\struct Point { x: int, y: int }
        \\let p = Point { x: 10, y: 20 };
        \\print(p.x);
        \\print(p.y);
        \\
        \\let p2 = Point { x: 30, y: 40 };
        \\print(p2.x);
        \\print(p2.y);
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse
    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    try sema.analyze(program);

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    const code = try codegen.generate(program);

    // Prepare output directory
    const out_dir_name = "zig-out/test_tmp_structs";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "test_program_structs";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return error.CompilationFailed;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    try std.testing.expectEqual(run_result.term.Exited, 0);
    // 10, 20, 30, 40
    try std.testing.expectEqualStrings("10\n20\n30\n40\n", run_result.stdout);
}

test "integration floats" {
    if (builtin.os.tag != .macos and builtin.os.tag != .linux) return error.SkipZigTest;

    const source =
        \\let pi = 3.14159;
        \\print(pi);
        \\
        \\let r = 2.0;
        \\let area = pi * r * r;
        \\print(area);
        \\
        \\if (area > 10.0) {
        \\    print(1.0);
        \\} else {
        \\    print(0.0);
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse
    var parser = Parser.init(allocator, source);
    const program = try parser.parse();

    // Diagnostics
    var diagnostics = Diagnostics.init(allocator);

    // Sema
    var sema = try Sema.init(allocator, &diagnostics);
    try sema.analyze(program);

    // Codegen
    var codegen = try LLVMCodegen.init(allocator);
    const code = try codegen.generate(program);

    // Prepare output directory
    const out_dir_name = "zig-out/test_tmp_floats";
    try std.fs.cwd().makePath(out_dir_name);
    const out_dir = try std.fs.cwd().openDir(out_dir_name, .{});

    // Write LLVM IR
    try out_dir.writeFile(.{ .sub_path = "out.ll", .data = code });

    // Write runtime
    try out_dir.writeFile(.{ .sub_path = "runtime.c", .data = runtime_c });

    // Compile
    const out_exe = "test_program_floats";
    var cc_args = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer cc_args.deinit(allocator);
    try cc_args.append(allocator, "cc");

    if (builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        try cc_args.append(allocator, "-arch");
        try cc_args.append(allocator, "x86_64");
    }

    try cc_args.append(allocator, "-o");
    try cc_args.append(allocator, out_exe);
    try cc_args.append(allocator, "out.ll");
    try cc_args.append(allocator, "runtime.c");

    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = cc_args.items,
        .cwd = out_dir_name,
    });

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed:\n{s}\n{s}\n", .{ compile_result.stdout, compile_result.stderr });
        return error.CompilationFailed;
    }

    // Run
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{try std.fs.path.join(allocator, &.{ ".", out_exe })},
        .cwd = out_dir_name,
    });

    try std.testing.expectEqual(run_result.term.Exited, 0);
    try std.testing.expectEqualStrings("3.141590\n12.566360\n1.000000\n", run_result.stdout);
}
