# Stack Lang

Stack Lang is the new hot thing (makes C obsolete!!!)

### 1. Abstract

This project is a compiler for a toy language. The main feature of this
language is the stack based style of programming. It will include functions
that work with a stack only. For example all variables will be located on the
stack, and the functions (or operators) will use elements from the stack and
then write them back on the stack.

### 1. Language Spec

This is still in progress. Just trying to come up with something that is both
fun and easy to code in.

#### 1.1. Comments

Line comments are prefixed by `--` symbol.

#### 1.2. Program

A `stack` program is defined as a list of `Data Types` and `Functions`. The
entry point of a `stack` program is the `main` function.

#### 1.3. Stack Data Types

##### 1.3.1. int

The `int` data type is implemented by default by the language. This is a 8 byte
signed int. You can think of it as a `int64` type in C.

##### 1.3.2. bool

The `bool` data type is also provided by the stack compiler. It can have the
values of `true` and `false`. These can be thought as the constructors of the
boolean type.

##### 1.3.3. string

The `string` data type is part of the stack compiler. String in stack are
defined using the `"` (quotes). Strings can be escaped using the `\`
(backslash) character. For example `"Hello, World\n"` is a valid string in
stack.

##### 1.3.4. ptr

The `ptr` data type is used to represent a pointer. It's value is an int64. All
data types will be able to be converted between and into `ptr` for easy memory
management.

#### 1.4. Functions

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

##### 1.4.1. `dup`

```stack
func dup (a) (a, a) in
    ...
end
```

The dup function can be used to duplicate `any` item that is currently on the
stack. The type of the `a` parameter is dependent of the context where this
function is used. The stack compiler will use inference to determine the type
of `a`.

##### 1.4.2. `swp`

```stack
func swp (a, b) (b, a) in
    ...
end
```

The swap function can be used to swap the first two items on the stack. The
types are also generic `a` and `b` and are infered from the context in the
compile phase.

##### 1.4.3. `rot`

```stack
func rot (a, b, c) (b, c, a) in
    ...
end
```

The rotate (clockwise) function can be used to rotate  the first three items on
the stack. Rotate will take the last item and put it in the front.

##### 1.4.3. `rot4`

```stack
func rot' (a, b, c) (c, a, b) in
    rot rot
end
```

The rotate4 (clockwise) function can be used to rotate the first four items on
the stack.

##### 1.4.4. `pop`

```stack
func pop (a) () in
    ...
end
```

The pop function can be used to remove items from the stack.

##### 1.4.5. `ptr.alloc`

```stack
func ptr.alloc (int) (ptr) in
    ...
end
```

The allocate function will initialize a ptr buffer with size given as argument.
The size is given in bytes.

##### 1.4.6. `ptr.+`

```stack
func ptr.+ (ptr, int) (ptr) in  -- offset address
    ...
end
```

The offset function will add an offset to an address.

##### 1.4.7. `data.&`

```stack
func data.& (a) (ptr) in
    ...
end
```

The ref function will generate a reference to a data. This function is
currently implemented by the compiler for each data type and it will be for
example `ptr.&` or `int.&`. (in other terms it will box the data into a
container and give us the new address of that).

##### 1.4.8. `data.*`

```stack
func data.* (ptr) (a) in
    ...
end
```

The deref function will dereference a ptr value (in other words it will unbox a
ptr and give us the value which should the data contained by the box). This
function is also implemented by the compiler for each data type.

##### 1.4.9. `ptr.@`

```stack
func ptr.@ (ptr, ptr) () in
    ...
end
```

The copy function will copy one byte from the `src` to the `dest` pointers.
Function should be called like

```stack
dest src ptr.@
```

##### 1.4.10. `syscall3`

There are syscall function implemented for all number of argument in a syscall.

```stack
func syscall3 (a, b, c, int) (int) in
    ...
end
```

The last argument of a syscall function is the syscall number.

#### 1.5. Expressions

Expressions in stack are used in the body of the functions as a sequence of
operations that work on the stack.

##### 1.5.1. Literals

The most basic expression in stack is the literal. These can be `int`, `bool`
or `string` (maybe others soon) and are added on the stack by just using them.

##### 1.5.2. Function calls

To call functions in stack you need to just use the name of the function as an
expression.

##### 1.5.2. Conditionals

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
if the condition is met. Some examples

```stack
-- the abs function returns the absolute value
func abs (int) (int) in
    dup 0 < if 0 swp - fi
end
```

#### 1.5. Custom data types

In `stack` you can define data types (or structures) using the `data` keyword.

We can look at an example of implementing the `ivec2` data structure.

```stack
data ivec2 (int x, int y)
```

The stack compiler will create functions that convert the data type from and to
a `ptr`. This way you can easily implement the `init` and `getters` of the data
type.

```stack
func ptr.ivec2 (ptr) (ivec2) in
    ...
end

func ivec2.ptr (ivec2) (ptr) in
    ...
end
```

Some examples of using these `ptr` conversions are offseting the data pointer
to access fields (or initialize them). The pattern is usually

```stack
ivec2.ptr ptr.int 0 + int.ptr
```

which gives you the pointer to the first field. Then you can use store and
deref to modify or read the value.

#### 1.6. Entrypoint

The entrypoint of a stack lang program is the main function which should be
define as

```stack
func main () (int) in
    ...
end
```

The main function is expected to have one `int` on the stack that represents
the exit code.

#### 1.7. Preprocessor

##### 1.7.1 Imports

WIP: Currently the imports are work in progress and should not be used. I need
them only to keep common code in stdlib.

To import another file in a stack program you can use the `@import` macro and
then specify the name of the module.

```stack
@import stdlib
```

Stack will look for the module file in the current folders in order:
- `.` in the current directory
- `lib` in the lib folder of the current project
- `/usr/lib` in the lib folder of the os or where stack is installed (this is WIP)

Supports adding multiple paths by using the `STACK_PATH` environment variable.

```console
export STACK_PATH=mymodule:lib2:$STACK_PATH
```

### 2. Quickstart

You can use `nix` to easily build the stack compiler.

```console
nix build
./result/bin/slc --help
```

Or you can just use `make`.

```console
make build
./main --help
```
