# Stack Lang

This project is a compiler for a toy language. The main feature of this
language is the stack based style of programming. It will include functions
that work with a stack only. For example all variables will be located on the
stack, and the functions (or operators) will use elements from the stack and
then write them back on the stack.

### Quick hack for the start

This is just to get a feeling on how to use stack based stuff with assembly and
stuff like that. The Spec is actually the real language I am trying to build.

For example imagine the following program

```stack
0
```

will simply return the status code of 0. All `stack` programs must end with a
single int number on the stack, which represents the status code.

Operators that can be used with the `int` type are addition `+`, substract `-`,
multiplication `*`, int division `/` and modulo `%`.

The following program

```stack
1 2 +
```

would result in a status code of `3`. Tokens in `stack` can be either variables
with values or functions. Functions will consume some values from the stack
and produce some other values on the stack.

### Language Spec

This is still in progress. Just trying to come up with something that is both
fun and easy to code in.

#### Comments

Line comments are prefixed by `--` symbol.

#### Data Types

In `stack` you can define data types (or structures) using the `data` keyword.
The `int` data type is implemented by default by the language.

We can look at an example of implementing the `ivec2` data structure.

```stack
-- data int
data ivec2 (int x, int y)
```

To create a new `ivec2` you will need to push 2 `int`'s on the stack and then
call the constructor.

```stack
-- int int -> ivec2
1 2 ivec2
```

#### Functions

Functions are defined using the `func` keyword. After the `func` keyword comes
the name of the function and then the list of arguments and return values. They
are separated by the `->` (arrow) symbol. To specify the start of the body of
the function we use the `in` keyword. A function is ended with the `end`
keyword.

The compiler will automatically generate accessor functions for the fields of a
structure. All the structures and types are immutable.

You can think of the accessors as

```stack
-- take the first field of the structure aka `x`
func ivec2.x ivec2 -> int in
    ...
end

-- take the first field of the structure aka `y`
func ivec2.y ivec2 -> int in
    ...
end
```

The convention is to prefix the name of the function with the name of the data
type and `.` since this makes it look like most programming languages.

Example of using the created data structure

```stack
-- dot product on ivec2 structure; it returns an int
func ivec2.dot ivec2 ivec2 -> int in
    dup ivec2.x swp ivec2.y rot
    dup ivec2.x swp ivec2.y rot
    * swp rot
    *
    +
end
```

the `dup`, `swp`, `rot`, `*` and `+` are functions that are defined in the
compiler itself.

### Quickstart

To build the compiler you will need the following dependencies:
- clang

```console
make build
```
