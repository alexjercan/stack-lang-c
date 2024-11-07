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

##### 1.4.3. `rot'`

```stack
func rot' (a, b, c) (c, a, b) in
    ...
end
```

The rotate' (counter clockwise) function can be used to rotate the first three
items on the stack. Rotate' will take the first item and push it behind the
first two.

##### 1.4.4. `pop`

```stack
func pop (a) () in
    ...
end
```

The pop function can be used to remove items from the stack.

##### 1.4.5. `+`

```stack
func + (int, int) (int) in
    ...
end
```

The `+` plus function will consume 2 int's on the stack and produce an int on
the stack by adding the values of them.

##### 1.4.6. `*`

```stack
func * (int, int) (int) in
    ...
end
```

The `*` plus function will consume 2 int's on the stack and produce an int on
the stack by multiplying the values of them.

##### 1.4.7. `>`

```stack
func > (int, int) (bool) in
    ...
end
```

The `>` "greater than" function will compare two int values from the stack.
That said, `1 0 >` will return `true`.

##### 1.4.8. `<`

```stack
func < (int, int) (bool) in
    ...
end
```

The `<` "less than" function will compare two int values from the stack. That
said, `0 1 <` will return `true`.

##### 1.4.9. `=`

```stack
func = (int, int) (bool) in
    ...
end
```

The `=` "equals" function will compare two int values from the stack. That
said, `1 1 =` will return `true`.

##### 1.4.10. `string.out`

```stack
func string.out (string) () in
    ...
end
```

The out function will print a `string` to stdout.

##### 1.4.11. `string.concat`

```stack
func string.concat (string, string) (string) in
    ...
end
```

The concat function will print concatenate the two strings in order and put the
result on the stack.

```stack
"Hello, " "World\n" string.concat
-- "Hello, World\n"
```

##### 1.4.11. `string.substr`

```stack
func string.substr (string, int, int) (string) in
    ...
end
```

The substr function will create a substring starting at `i` with length `L` and
put it on the stack.

```stack
"Hello, World\n" 1 3 string.substr
-- "ell"
```

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
    dup 0 >
    if -1 * fi
end
```

The `if` statement can be used to get a rudimentary for loop using recursion.
This is an example of a function that shows the characters from `i` to `n`.

```stack
-- this function will be used to recurse
-- args: i, n
func show_n (int, int) () in
    dup -- i, n, n
    rot -- n, n, i
    dup -- n, n, i, i
    rot -- n, i, i, n
    > -- n, i, bool
    if -- n, i
        dup -- n, i, i
        out -- n, i
        1 + -- n, (i + 1)
        swp -- (i + 1), n
        show_n -- ()
    else -- n, i
        -- making sure the type is the same
        pop pop -- ()
    fi
end
```

#### 1.5. Custom data types

In `stack` you can define data types (or structures) using the `data` keyword.

We can look at an example of implementing the `ivec2` data structure.

```stack
data ivec2 (int x, int y)
```

The stack compiler will automatically generate a few functions for your
structure.

To create a new `ivec2` you will need to push 2 `int`'s on the stack and then
call the constructor.

```stack
-- (int int) (ivec2)
1 2 ivec2
```

The compiler will automatically generate accessor functions for the fields of a
structure. All the structures and types are immutable.

You can think of the accessors as

```stack
-- take the first field of the structure aka `x`
func ivec2.x (ivec2) (int) in
    ...
end

-- take the first field of the structure aka `y`
func ivec2.y (ivec2) (int) in
    ...
end
```

The convention is to prefix the name of the function with the name of the data
type and `.` since this makes it look like most programming languages.

Example of using the created data structure

```stack
-- dot product on ivec2 structure; it returns an int
func ivec2.dot (ivec2 ivec2) (int) in
    dup ivec2.x swp ivec2.y rot
    dup ivec2.x swp ivec2.y rot
    * swp rot
    *
    +
end
```

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
