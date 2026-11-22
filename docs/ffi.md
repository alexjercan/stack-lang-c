# C functions

An `extern` declares a C function. The assembler makes a wrapper with the
System V AMD64 calling convention.

```stack
extern c.add (int, int) (int) "add"
```

- Arguments and the result of type `int`, `ptr` or `bool` are implemented. In
  C, `int` maps to `int64_t`, `ptr` to `void *`, and `bool` to `int64_t`
  containing `0` or `1` (not C `_Bool`). Other types are undefined behavior.
  The assembler does not reject them.
- An extern with no result or one result is implemented. More than one result
  is undefined behavior. The assembler does not reject it.
- The first argument is the deepest stack item. Arguments 1 to 6 go in `rdi`,
  `rsi`, `rdx`, `rcx`, `r8` and `r9`. Other arguments go on the machine stack.
- The wrapper aligns `rsp` to 16 bytes before the call.
- The symbol must be printable ASCII without spaces or quotes. Many externs can
  use the same symbol.
- The type checker accepts all types. The assembler rejects bad symbols before
  it writes output.

The program has no C runtime startup. Do not use libc, thread local storage
or the stack protector in the C code. Every declared extern must be linked,
also when the program does not call it.

```fish
printf '#include <stdint.h>\nint64_t add(int64_t a, int64_t b) { return a + b; }\n' > add.c
printf 'extern c.add (int, int) (int) "add"\nfunc main () (int) in 40 2 c.add end\n' > add.stack
cc -c -O2 -fno-stack-protector add.c -o add.o
out/slc add.stack -a > add.asm
fasm add.asm add-stack.o
ld -z noexecstack add-stack.o add.o -o add
./add; echo $status
```

The last command writes `42`.
