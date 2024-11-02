# Stack Lang

This project is a compiler for a toy language. The main feature of this
language is the stack based style of programming. It will include functions
that work with a stack only. For example all variables will be located on the
stack, and the functions (or operators) will use elements from the stack and
then write them back on the stack.

### Language Spec

This is still in progress. Just trying to come up with something that is both
fun and easy to code in.

#### Comments

Line comments are prefixed by `--` symbol.

#### Program

A `stack` program is defined as a list of `Data Types` and `Functions`. The
entry point of a `stack` program is the `main` function.

#### Data Types

In `stack` you can define data types (or structures) using the `data` keyword.
The `int` data type is implemented by default by the language.

We can look at an example of implementing the `ivec2` data structure.

```stack
data ivec2 (int x, int y)
```

To create a new `ivec2` you will need to push 2 `int`'s on the stack and then
call the constructor.

```stack
-- (int int) (ivec2)
1 2 ivec2
```

#### Functions

Functions are defined using the `func` keyword. After the `func` keyword comes
the name of the function and then the list of arguments and return values
delimited using parenthesis. To specify the start of the body of the function
we use the `in` keyword. A function is ended with the `end` keyword.

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

the `dup`, `swp`, `rot`, `*` and `+` are functions that are defined in the
compiler itself.

#### Entrypoint

The entrypoint of a stack lang program is the main function which should be
define as

```stack
func main () (int) in
    ...
end
```

The main function is expected to have one `int` on the stack that represents
the exit code.

### Quickstart

```console
nix build
./result/bin/slc --help
```
