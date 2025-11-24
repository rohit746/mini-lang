# Mini Compiler in Zig

A simple compiler for a custom programming language ("Mini") written in Zig. This project demonstrates the core stages of a compiler: Lexing, Parsing, Semantic Analysis, and Code Generation (x86-64 Assembly).

## Features

- **Data Types**:
  - Integers (`i64`)
  - Booleans (`true`, `false`)
  - Strings (String literals)
- **Variables**: `let` declarations and assignment.
- **Arithmetic**: `+`, `-`, `*`, `/`.
- **Logic**: `&&`, `||`, `!`, `==`, `!=`, `<`, `<=`, `>`, `>=`.
- **Control Flow**:
  - `if` / `else` statements.
  - `while` loops.
- **Functions**:
  - Function declarations (`fn`).
  - Function calls with arguments.
  - `return` statements.
- **I/O**: Built-in `print()` function.

## Architecture

The compiler is structured into four main phases:

1.  **Lexer (`src/lexer.zig`)**: Tokenizes the source code into a stream of tokens (keywords, identifiers, literals, operators).
2.  **Parser (`src/parser.zig`)**: Consumes tokens to build an Abstract Syntax Tree (AST) representing the program structure. Implements operator precedence.
3.  **Semantic Analysis (`src/sema.zig`)**: Traverses the AST to check for errors (undefined variables, type mismatches, scope resolution).
4.  **Code Generation (`src/codegen.zig`)**: Traverses the AST to generate x86-64 assembly code (AT&T syntax).

## Building and Running

### Prerequisites

- [Zig Compiler](https://ziglang.org/download/) (tested with 0.13.0+)
- A C compiler (like `clang` or `gcc`) for linking the runtime.

### Build

To build the compiler executable:

```bash
zig build
```

The executable will be located at `zig-out/bin/zig_test`.

### Run a Program

To compile and run a Mini program:

```bash
zig build run -- test.mini
```

Or directly using the executable:

```bash
./zig-out/bin/zig_test test.mini
```

### Run Tests

To run the project's unit and integration tests:

```bash
zig build test
```

## Language Syntax

### Variables

```mini
let x = 10;
let s = "hello";
let b = true;
```

### Control Flow

```mini
if (x < 20) {
    print(x);
} else {
    print(0);
}

while (x > 0) {
    x = x - 1;
}
```

### Functions

```mini
fn add(a, b) {
    return a + b;
}

let result = add(5, 10);
print(result);
```

### Unary Operators

```mini
let y = -10;
let not_valid = !true;
```

## Implementation Details

- **Memory Management**: Uses Zig's `ArenaAllocator` for efficient memory management during compilation.
- **Codegen**: Generates a `.s` assembly file and a `runtime.c` file (for `printf`), then invokes the system C compiler (`cc`) to link them into a final executable.
- **Scopes**: Supports nested scopes for variables (block scoping).

## License

MIT
