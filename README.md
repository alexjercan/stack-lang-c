# Stack Lang

Stack Lang is the new hot thing (makes C obsolete!!!)

Backlog:
- [ ] generate more tests for all cases ~20 each (lexer, parser, preprocessor, typechecker)
- [ ] better documentation
- [ ] optimizations for speed => we don't need everything to be a function we can inline most things
- [ ] refactor stack.stack to use `while` and `match`
- [ ] memory management => custom allocator that free's unused variables
- ...

## 1. Introduction

This manual describes the programming language `Stack`. Stack is a stack based
programming language. The language uses data structures and functions to define
programs. It includes static typing with inference.

`Stack` programs are sets of functions with a main entrypoint. `Stack` programs
can also define custom data types, which can be used to describe how memory
should be interpreted. A function in `Stack` is a set of expressions that
operate on a stack.

`Stack` is type safe: the stack is guaranteed to have the expected types in the
body of a function. `Stack` is currently weakly typed. It allows you to convert
any type to any other type by using pointers directly, which can lead to
runtime errors if not used carefully.

## 2. Quickstart

This repository includes a `Stack` compiler that can be used for each step of
converting a `.stack` file into an executable.

To build the compiler, use `nix` or `make`.

```console
nix build
```

or

```console
make
```

To compile a `Stack` program using the `slc` compiler:

```console
slc [ -o main ] main.stack
```

[Documentation](docs/)

## 3. Stack

The stack stores pointers to data that lives on the heap.

Type checking will make sure that the stack is valid for a program before
allowing it to be compiled.

## 3. Features

`Stack` programs are set of features. Features can be functions, data types and
macros.

## 3.1. Functions

`Stack` functions are sets of expressions that operate on the stack. Functions
can add and remove items from the stack.

Functions will define the expected state of the stack before and after the
execution.

```stack
func <name> (<type_i1>, <type_i2>, ...) (<type_o1>, <type_o2>, ...) in
    <body>
end
```

The function names can contain any name character (alpha numeric and symbols
with the exception of `(),`). Functions may not be redefined.

The input of the function is given as the list `(<type_i1>, <type_i2>, ...)`
and the output of the function as `(<type_o1>, <type_o2>, ...)`. The body of a
function will be just a list of expressions. After applying all the
expressions, the stack must go from the input to the output state.

## 3.2. Data Types

`Stack` data types are similar to C structures. They are used to store data, and
will give an overview on how the memory will be used.

```stack
data <type>
    ( <type1> <field1>
    , <type2> <field2>
    )
```

The data type names can contain any name character (alpha numeric and symbols
with the exception of `(),`). Data types may not be redefined.

The data types contain a set of `Data Fields`. Each field has a type and a
name. The types are used to compute the size of each field. The names are used
to be able to offset into memory.

`Stack` data types will have convenience functions generated for initializing
them from a stack that contains all the fields in order. Data types will also
have field getters and setters.

```stack
-- constructor for the data type
func <type>.init (<type1>, <type2>) (<type>) in ... end

-- field getters for the data type
func <type>.<field1> (<type>) (<type1>) in ... end
func <type>.<field2> (<type>) (<type2>) in ... end

-- field setters for the data type
func <type>.<field1>.set (<type>, <type1>) () in ... end
func <type>.<field2>.set (<type>, <type2>) () in ... end
```

## 3.3. Consts

Constants in `Stack` are macros that are expanded in the AST of the program
during preprocessor stage. Constants are not directly type checked. They need
to make sense in the place where they are used.

For example

```stack
const <name> <body>
```

will be expanded as as `<body>` in the place where it is used.

Constant names are unique in the whole program. A constant may not be defined
twice, in the same file or in different files. A constant may not use the name
of a builtin constant (for example `int.sizeof`) or of a constant generated
from a data type (for example `<type>.sizeof`). Generated names are reserved
regardless of the order of the declarations.

Constant definitions must be acyclic. A constant cannot use itself directly or
through other constants, including constants generated from data types. The
preprocessor checks the original definitions of each file for cycles before it
checks for duplicate names.

## 3.4. Imports

Import macros were added to keep the code organized into modules.

To import another file in a stack program you can use the `@import` macro and
then specify the name of the module.

```stack
@import "stdlib"
```

Stack will look for the module file in the current folders in order:
- `.` in the current directory
- `lib` in the lib folder of the current project
- `/usr/lib` in the lib folder of the os or where stack is installed (this is WIP)

Supports setting the path by using the `STACK_HOME` environment variable.

```console
export STACK_HOME=.
```

Each file is loaded only one time per compilation. The compiler identifies a
file by the device and inode of the open file. Different paths to the same file
(`mod`, `./mod`, an absolute path, a symbolic link, or a hard link) are one
file. The main input file is also registered, so an import of the main file is
ignored. Repeated imports, diamond imports, and import cycles do not load a
file again. Diagnostics and `__file__` use the path that loaded the file first.

When the main input is read from standard input, the compiler uses the file
behind descriptor 0. If standard input is redirected from a file, an import of
that file is ignored. If standard input is a pipe, the piped source is a
different file from every import.

Two different files that define the same function, data type, or constant are
an error, also when they have the same content.

## 4. Stack Data Types

### 4.1. int

The `int` data type is implemented by default by the language. This is a 8 byte
signed int. You can think of it as a `int64` type in C.

### 4.2. bool

The `bool` data type is also provided by the stack compiler. It can have the
values of `true` and `false`. These can be thought as the constructors of the
boolean type.

### 4.3. ptr

The `ptr` data type is used to represent a pointer. It's value is an int64. All
data types will be able to be converted between and into `ptr` for easy memory
management (by referencing and dereferencing).

### 4.4. string

The `string` data type is part of the stack compiler. String in stack are
defined using the `"` (quotes). Strings can be escaped using the `\`
(backslash) character. For example `"Hello, World\n"` is a valid string in
stack.

The `string` data type is part of the standard library. This data type is a
good example on why `data` is useful. By default a string will be allocated on
the heap as a 16 bytes sized block. The first 8 are used by the string length
and should be interpreted as an int. The next 8 are a pointer to a list of
bytes with the specified length. The `string` data type makes working with this
much easier.

## 5. Stack Functions

Functions are defined using the `func` keyword. After the `func` keyword comes
the name of the function and then the list of arguments and return values
delimited using parenthesis. To specify the start of the body of the function
we use the `in` keyword. A function is ended with the `end` keyword.

For example we can take a look at a function that adds three numbers.

```stack
func add3 (int, int, int) (int) in
    + +
end
```

This function uses the predefined `+` (plus) function that takes 2 ints on the
stack as input and produces an int on the stack.

Functions in stack are called by just using their name in the body of another
function.

### 4.1. `dup`

```stack
func dup (a) (a, a) in
    ...
end
```

The dup function can be used to duplicate `any` item that is currently on the
stack. The type of the `a` parameter is dependent of the context where this
function is used. The stack compiler will use inference to determine the type
of `a`.

> **_NOTE:_**  This function will duplicate the pointer to the `a`, so changes
> to the first item will reflect in the second one as well. To create an actual
> copy you will need to allocate and memcpy the item.

### 4.2. `swp`

```stack
func swp (a, b) (b, a) in
    ...
end
```

The swap function can be used to swap the first two items on the stack. The
types are also generic `a` and `b` and are infered from the context in the
compile phase.

### 4.3. `rot`

```stack
func rot (a, b, c) (b, c, a) in
    ...
end
```

The rotate (clockwise) function can be used to rotate  the first three items on
the stack. Rotate will take the last item and put it in the front.

### 4.4. `rot4`

```stack
func rot' (a, b, c) (c, a, b) in
    rot rot
end
```

The rotate4 (clockwise) function can be used to rotate the first four items on
the stack.

### 4.5. `pop`

```stack
func pop (a) () in
    ...
end
```

The pop function can be used to remove items from the stack.

### 4.6. `pick`

```stack
func pick (int) (ptr) in
    ...
end
```

The pick function is used to copy one item from the stack given the index (from
end). This function is a good example on why the language is currently weakly
typed. It does not care about the type of the item from the stack, it will box
it into a ptr. The decision to which type to convert it after is left to the
developer.

### 4.7. `ptr.alloc` and `ptr.free`

```stack
func ptr.alloc (int) (ptr) in
    ...
end
```

The allocate function will initialize a ptr buffer with size given as argument.
The size is given in bytes.

```stack
func ptr.free (ptr) () in
    ...
end
```

The free function releases a block returned by `ptr.alloc`.

### 4.8. `ptr.+`

```stack
func ptr.+ (ptr, int) (ptr) in  -- offset address
    ...
end
```

The offset function will add an offset to an address.

### 4.9. `ptr.@`

```stack
func ptr.@ (ptr, ptr, int) (ptr) in
    ...
end
```

The copy function will copy `n` bytes from the `src` to the `dest` pointers.
Function should be called like

```stack
dest src n ptr.@
```

### 4.10. `.&`

```stack
func <data>.& (<data>) (ptr) in
    ...
end
```

The compiler generates `<data>.&` for each data type. It returns the address
of a value, for example `point.&`.

### 4.11. `.*`

```stack
func <data>.* (ptr) (<data>) in
    ...
end
```

The compiler generates `<data>.*` for each data type. It accesses a value
at an address, for example `point.*`.

### 4.12. `syscall1`

There are syscall function implemented for all number of argument in a syscall.

```stack
func syscall1 (a, int) (int) in
    ...
end
```

### 4.13. `syscall3`

There are syscall function implemented for all number of argument in a syscall.

```stack
func syscall3 (a, b, c, int) (int) in
    ...
end
```

The last argument of a syscall function is the syscall number.

### 4.14. math

There are also the well known functions for math operations `+`, `-`, `*`, `/`,
`%`, `|`, `&`, `^`, `>>`, `<<`. These functions are used on the `int` data type.

### 4.15. compare

There are also functions for comparing numbers `>`, `<`, `=`. These functions
will return `bool`.

Other functions that can be derived from the basic math or compare functions
are available in the `stdlib` module.

## 5. Expressions

Expressions in stack are used in the body of the functions as a sequence of
operations that work on the stack.

The blocks `if`, `match` and `while` can nest at most 2048 levels deep. A
deeper block is a parser error, because the compiler recurses for each level.

### 5.1. Literals

The most basic expression in stack is the literal. These can be `int`, `bool`
or `string` and are added on the stack by just using them.

### 5.2. Function calls

To call functions in stack you need to just use the name of the function as an
expression.

### 5.3. Conditionals

The `if` statement can be thought as a function that takes in a `bool` and
produces some changes on the stack. The syntax is like following

```stack
if ... else ... fi
```

where `...` can be a list containing any amount of expressions (even 0 if you
are like that).

Another feature of stack is that the `else` case can be omitted. This will
still be valid

```stack
if ... fi
```

This construction can be thought of doing some extra operations on the stack,
if the condition is met. Some examples in the `./examples/`

Another important aspect of `if` expressions is type checking. The stack will
need to have the same layout regardless of the path taken. That means that each
branch must generate the same types on the stack.

### 5.4. Match

The `match` statement can be though as a pattern match operation that takes in
some items from the stack, given as arguments in the match definition and
giving you scoped variables that can be used to push back those values in any
order. The syntax is like the following

```stack
match a b c ... in
    ...
end
```

where `a`, `b`, `c` can be any names. The scoped variable names must be unique
and cannot be the same as any function name. The variables also cannot be
shadowed by other variables in inner matches. See some examples in the
`./examples/`.

The variables will be type checked against the stack. You also cannot define
more variables than items on the stack. The body of a match can contain any
expressions. Binding names such as `a`, `b`, and `c` are separate from generic
type parameters. They do not change function type inference.

### 5.5 While

The `while` statement is a conditional block that can be used as an alternative
to a recursive calling. It is composed of two parts, the condition evaluation
and the loop body.

```stack
while
    ... some condition ... loop
    ... some stuff do do ...
pool
```

The while loop will execute the condition at each iteration to verify the truth
and based on that it will execute one iteration of the body. To keep things
sane, the while loop requires that the input is the same as the output. This
means that the stack must be in the same state before and after executing a
while loop regardless of how many iteration it does.

For instance, say the stack before hitting the `while` keyword contains `a b
c`. This means that when the stack hits the `pool` keyword, is must contain `a
b c` as well. This will enforce that the stack has a clean structure no matter
how many iteration will happen.

IMPORTANT: The output of the while will be determined in the `while ... loop`.
This happens because if the condition is false, the while will break on `loop`.
This means that whatever `loop` has on the stack that will be what the while
loop will leave on the stack once it is done. That means that the input of the
`while` and the output of the `pool` must be the same, but they don't matter to
the main stack, so it can be anything, but the `loop` expression is what will
be left on the stack. For example

```
0 1 2 while           -- int, int, int
    pop3 1 false loop -- int
    1 2 3
pool                  -- int, int, int
```

This while loop expects 3 `int`'s on the stack and the `pool` will need to
generate 3 `int`'s for this to work. But at the end of the while loop (which in
this case is just false, so it doesn't do anything), it will return a single
`int`. So the stack will contain a single `int` at the end of the execution of
the `while`.

## 6. Entrypoint

Every Stack program must define a `main` function. The recommended signature is

```stack
func main (int, ptr) (int) in
    ...
end
```

The input signature must be `(int, ptr)` or `()`. The return signature must
be exactly `(int)`, which represents the exit code. Generic parameters and
other types are not allowed in the entrypoint signature.

The compiler checks these requirements during type checking, before it emits
assembly. Library files without `main` must be imported into a program, not
compiled as standalone programs.

The stack starts with the `argc` and `argv` arguments on the stack. These can be
ignored if the main input is set to `()`. For example:

```stack
func main () (int) in
    0
end
```

> **_NOTE:_**  The `argc` and `argv` will still be pushed on the stack if the
> main input type is `()`. But since type checking assumes that the input to
> main is correctly defined, then it will be ignored. However, defining main as
> shown is recommended to avoid bugs.

The stack starts with `argc` and `argv` and must end with the `int` return
code.
