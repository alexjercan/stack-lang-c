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
programs. It includes static typing with inference and manual memory
management with `ptr.alloc` and `ptr.free`.

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
converting a `.stack` file into an executable. It also includes `stackls`, a
language server for `Stack` (see [7. Language Server](#7-language-server)).

To build the `Stack` compiler and `stackls` you can easily use `nix` with the
default build target, or just run make.

```console
nix build
```

or

```console
make
```

The default package has `bin/slc`, `bin/stackls` and the libraries in `lib`.
The packages `.#slc` and `.#stackls` have only one tool. `make` writes
`out/slc` and `out/stackls`. `make install` builds and installs both tools and
the libraries into `PREFIX`.

To compile a `Stack` program using the `slc` compiler:

```console
slc [ -o main ] main.stack
```

### Running tests

Run the test suite from the repository root:

```fish
nix develop --command bash scripts/checker.sh --all
```

Run one stage with `--lexer`, `--parser`, `--preprocessor`, `--typecheck`,
`--io`, `--import`, or `--runtime`. Add `--no-build` to use the existing `out/slc`.

Runtime tests check that generated assembly contains only printable ASCII.

```fish
nix develop --command bash scripts/checker.sh --preprocessor --no-build -v
```

Run the `stackls` protocol tests. `make test-lsp` builds the compiler and the
server from the current source, then runs `tests/lsp/protocol.py`:

```fish
nix develop --command make test-lsp
```

Run them on the Nix package:

```fish
nix flake check
```

## 3. Stack

The stack in a `Stack` program is an array with a fixed size of 64 KiB. The
stack is used to store pointers to data that lives on the heap. The heap grows
with `brk` until the system refuses more memory. The runtime does not give
memory back to the operating system.

The heap holds two kinds of memory:

- Scalar cells. Each `int`, `bool` or `ptr` value that the runtime computes
  goes into an 8 byte cell. Cells are in 64 KiB chunks. Cells have no header
  and you cannot free them.
- Blocks. Each `ptr.alloc` makes one block. An 8 byte header comes before the
  returned pointer. The header holds the block size, with the header, and bit
  0 is set while the block is in use. A block has at least 8 bytes of memory.

Cells and block pointers are 8 byte aligned. `ptr.free` puts a block in a free
list that is sorted by address and merges adjacent free blocks. `ptr.alloc`
uses the first free block that is large enough. It splits off the rest when at
least 16 bytes remain. Memory from `ptr.alloc` is always zero.

These errors write a message to stderr and exit with status 1:

- `Runtime error: negative allocation size`: `ptr.alloc` got a negative size.
- `Runtime error: out of memory`: the heap cannot grow.
- `Runtime error: invalid free` and `Runtime error: double free`: `ptr.free`
  found a problem (see `ptr.free`).

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

The free function gives a block from `ptr.alloc` back to the heap. Use the
exact pointer that `ptr.alloc` returned. `ptr.null` does nothing. The free is
shallow: memory that the block points to stays allocated. After the free, all
copies of the pointer and all pointers into the block are dangling. You must
not use them.

Nothing frees memory automatically. The standard library frees only memory
that it owns:

- `src old new ptr.realloc` allocates `new` bytes, copies the first `old` or
  `new` bytes (the smaller one), frees `src` and returns the new block. `src`
  must be `ptr.null` or a block from `ptr.alloc`. All pointers into `src` are
  dangling after the call.
- An empty array has no items block (`array.items` is `ptr.null`). When
  `array.append`, `array.insert`, `array.insert_many` or `array.extend` need
  more capacity, the items move to a new block and the old block is freed. The
  new capacity is two times the old capacity (`array.SIZE` for an empty
  array), or the new count if that is larger. Pointers from `array.get` and
  `array.last`, and headers that share the items of the array, are not valid
  after a growth. The item
  or source array can point into the destination array: the functions copy
  the source bytes before they move or free anything. An array that wraps
  borrowed memory, such as `argv`, must use `array.copy` before it grows.
- Temporary buffers of the standard library are freed. Except for realloc
  and array growth as described above, the functions do not free memory
  that the caller gives them.

The free checks are best effort. `ptr.free` stops the program for some
pointers that are not 8 byte aligned, not in the heap (for example the address
of a literal), with a bad header, or already free. It cannot find
all bad pointers. The result of a free of any other pointer, for example a
pointer into a block, a scalar cell, or a made up value, is not defined.

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

### 4.10. `data.&`

```stack
func data.& (a) (ptr) in
    ...
end
```

The ref function will generate a reference to a data. This function is
currently implemented by the compiler for each data type and it will be for
example `ptr.&` or `int.&`. (in other terms it will box the data into a
container and give us the new address of that).

### 4.11. `data.*`

```stack
func data.* (ptr) (a) in
    ...
end
```

The deref function will dereference a ptr value (in other words it will unbox a
ptr and give us the value which should the data contained by the box). This
function is also implemented by the compiler for each data type.

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

## 7. Language Server

`stackls` is a language server for `Stack`. It reads LSP messages on stdin,
writes them on stdout, and writes logs to stderr.

```console
stackls [-h | --help] [-v | --version] [--stdio]
```

The server supports hover. It advertises `"hoverProvider": true` and full
text synchronization (`"textDocumentSync": {"openClose": true, "change": 1}`).
Go to definition, references, completion and diagnostics are not supported.

The source is `stackls.stack`. It uses these libraries:

- `lib/rpc.stack`: message framing.
- `lib/lsp.stack`: JSON-RPC, the lifecycle, open documents and positions. It
  does not know `Stack`, and other programs can import it.
- `lib/json.stack`: JSON values.
- `lib/stack/service.stack` and `lib/stack/analysis.stack`: the `Stack`
  requests. They use the parts of the compiler: `lib/stack/lexer.stack`,
  `lib/stack/parser.stack`, `lib/stack/preprocessor.stack` and
  `lib/stack/typecheck.stack`. `stack.stack` uses the same files.

`tests/lsp/protocol.py` uses only the Python standard library. It acts as a
client: it starts one server per case, sends bytes, and checks the exit code
and every byte on stdout. The hover tests that import `stdlib` use
`../lib` from the directory of the server: the repository for `out/stackls`,
and the package for `result/bin/stackls`. You can run it on such a binary:

```fish
python3 tests/lsp/protocol.py result/bin/stackls
```

### 7.1. Protocol

Framing:

- Header lines end with `\r\n`, and an empty line ends the header.
- Header names do not use case. `Content-Length` must be one decimal number
  of at most 64 MiB, and it must occur one time. Other valid headers, for
  example `Content-Type`, are ignored.
- The header can have at most 8 KiB.
- A bad header, or the end of input inside a message, stops the server with
  exit code 1 and a message on stderr. The end of input between messages
  stops the server with exit code 0.

JSON-RPC:

- A body that is not valid JSON gets error `-32700` with `"id": null`. The
  server then reads the next message.
- A message that is not a valid request or notification gets error `-32600`.
  `jsonrpc` must be `"2.0"`, `method` must be a string, `params` must be an
  object, an array or `null`, and `id` must be a string or an integer that
  fits in 64 bits. Ids are sent back as they are.
- A message without `method` and with `result` or `error` is a response. The
  server ignores it.
- Notifications get no response. Unknown notifications are ignored.

Lifecycle:

- Before `initialize`, requests get `-32002`.
- `initialize` accepts `params` that are `null`, missing or an object.
  `clientInfo` is optional. When it is present, it must be an object with a
  string `name` and an optional string `version`, or the request gets
  `-32602`. Other fields are ignored.
- After `initialize`, unknown requests get `-32601`, and `initialize` again
  gets `-32600`.
- `shutdown` gets `null`. After it, requests get `-32600`.
- `exit` stops the server with code 0 after `shutdown`, and 1 if there was no
  `shutdown`.

Documents:

- `textDocument/didOpen`, `textDocument/didChange` and
  `textDocument/didClose` keep the text and the version of each open
  document. A change must have the full text. A change with a `range` is
  ignored and logged. Notifications with bad `params` are ignored and logged.
- The text of an open document is used in place of the file on disk, also
  when another file imports it. A URI that is not a `file://` URI can be
  open, but other files cannot import it.

Hover (`textDocument/hover`):

- Positions use UTF-16 code units, and lines end with `\n`, `\r\n` or `\r`.
  A character after the end of a line is the end of the line.
- The result has the code of the definition and a short note, and the range
  of the name. The content is Markdown with a `stack` code block when the
  first known kind in `capabilities.textDocument.hover.contentFormat` is
  `markdown`, else plain text.
- These names get a result: builtin functions and data types, the `func` and
  `data` definitions of the file and of its imports, the type names in
  signatures and fields, `T.&` and `T.*`, and the generated constants
  `T.init`, `T.sizeof`, `T.f`, `T.f.set` and `T.f.offset` with their stack
  effect. The getter `T.f` gives a value that aliases the field.
- These positions get `null`: literals, keywords, comments, match bindings
  and the names that they bind, constants of the program, type variables,
  names that do not resolve, and documents that are not open.
- Bad `params` get `-32602`.
- Imports resolve as in `slc`: `name.stack` relative to the directory where
  the server runs, then `$STACK_HOME/lib/name.stack`.
- If a file has a syntax error, the definitions before the error are used.
  Names after the error get `null`. A missing or broken import keeps the
  definitions that were loaded before it.

### 7.2. Known limitations

- Memory grows with each message. The `Stack` runtime puts each computed
  `int`, `bool` and `ptr` into a cell that it never frees (see
  [3. Stack](#3-stack)). The server frees each JSON tree, frame buffer,
  document text and the lists of an analysis with `ptr.free`, but cells and
  syntax trees stay. A measurement gave about 150 KB for each small request.
- The first hover after a change analyses the document again. Later hovers
  use the same analysis until a document opens, changes or closes. The server
  keeps the parsed imports and parses an import again only when its text
  changes. Measurements on x86-64 Linux: the first hover in a file that
  imports `stdlib` adds about 50 MB, and each hover after a change about
  5 MB. For a file that imports the compiler (`stackls.stack`) the first hover
  adds about 620 MB and each hover after a change about 50 MB. Restart the
  server to give the memory back.
- The server reads an import that is not open from disk at each analysis.
  It does not watch files, so a change on disk is seen at the next analysis.
- Imports resolve from the directory where the editor starts the server.
- The server handles one message at a time. `$/cancelRequest` is ignored.
- If the client closes stdout, the write gets `SIGPIPE` and the server stops.
- The server runs only on x86-64 Linux, as the `Stack` compiler.
