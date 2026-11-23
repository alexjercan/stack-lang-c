# Standard library modules

`@import "stdlib"` imports all the modules below, so programs that use it do
not change. A program can also import only the modules that it uses. Each
module imports the modules that it needs, and the compiler loads each file one
time.

| Import | Contents | Imports |
| --- | --- | --- |
| `linux` | System calls (`sys.*`) and their constants | - |
| `std/core` | Stack shuffles (`dup2`, `over`, ...), `<=`, `>=`, `not`, `and`, `or`, `bool.show` | - |
| `std/byte` | `BYTE_*` constants, `byte.init`, byte classes | `std/core` |
| `std/ptr` | `ptr.null`, `ptr.=`, `ptr.realloc`, `ptr.memcmp`, `ptr.strlen` | `std/core`, `std/byte` |
| `std/string` | `data string`, `string.*`, `byte.chr` | `std/core`, `std/byte`, `std/ptr` |
| `std/int` | `min`, `abs`, `mod`, `digit.show`, `int.show`, `int.read` | `std/core`, `std/byte`, `std/string` |
| `std/io` | `STDIN`, `STDOUT`, `STDERR`, `stdlib.f*` file functions | `linux`, `std/core`, `std/string` |
| `std/panic` | `abort`, `here`, `panic`, `unwrap`, `todo` | `linux`, `std/core`, `std/string`, `std/int`, `std/io` |
| `std/stdio` | `string.stdin`, `string.stdout`, `string.stderr` | `std/io`, `std/panic` |
| `std/error` | `data error`, `error.*` | `std/core`, `std/string`, `std/panic`, `std/stdio` |
| `std/array` | `data array`, `array.*` | `std/core`, `std/ptr`, `std/string`, `std/panic` |
| `cstr` | `cstr.from`, `cstr.array`, `cstr.array.free` | `std/core`, `std/byte`, `std/ptr`, `std/string`, `std/panic`, `std/array` |
| `std/os` | `os.env.get`, `os.random.hex`, `os.tempdir.create` | `linux`, `std/core`, `std/byte`, `std/string`, `cstr` |
| `process` | `process.*`: find, run, and wait for programs | `linux`, `std/core`, `std/string`, `std/io`, `std/array`, `cstr`, `std/os` |

`stdlib` does not import `process`, `json`, `rpc`, or `lsp`.

The compiler looks for an import in the current directory first, then in
`$STACK_HOME/lib`. A file `std/string.stack` in the current directory hides
the module `std/string`, and a file `cstr.stack` hides `cstr`.

`here`, `panic`, `unwrap`, and `todo` are constants. A panic message shows the
place where the constant is used. A panic in a library function, for example
`error.unwrap`, shows the place in its module, such as `lib/std/error.stack`.
