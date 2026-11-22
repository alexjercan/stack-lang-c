# Development

```fish
nix develop --command bash scripts/checker.sh --all
```

Run one stage with `--lexer`, `--parser`, `--preprocessor`, `--typecheck`,
`--io`, `--import`, `--ffi`, `--driver`, or `--runtime`. Add `--no-build` to use
the existing `out/slc`. `--ffi` and `--driver` also need a C compiler (`CC`,
default `cc`). `--driver` also needs `cc` and `ar` in `PATH`.

Runtime tests check that generated assembly contains only printable ASCII.

```fish
nix develop --command bash scripts/checker.sh --preprocessor --no-build -v
nix develop --command make test-lsp
nix flake check
```

Run the tests of the Nixvim module. `eval` evaluates the module and builds
nothing. `headless` builds Neovim and checks filetype, syntax, LSP and hover.
`NIXVIM` selects the Nixvim flake. Use the revision of your configuration:

```fish
env NIXVIM=github:nix-community/nixvim/<rev> tests/nixvim/run.sh eval
env NIXVIM=github:nix-community/nixvim/<rev> tests/nixvim/run.sh headless
```

The tests use `path:` of the repository, so untracked files are included.
Set `STACK` to use another reference of this flake.

## Build programs

By default, `slc` compiles a program to an executable. The default output is
`a.out`. `-a` writes the assembly to stdout, or to the file of `-o`.

```fish
out/slc main.stack -o main
out/slc -a main.stack -o main.asm
```

The driver works only on Linux x86_64:

1. It makes a new directory with mode `0700` in `TMPDIR` (or `/tmp` when
   `TMPDIR` is not an absolute path) and writes the assembly into it.
2. It runs `fasm` on the assembly. The output of `fasm` on stdout is
   discarded. Its errors go to stderr.
3. It runs `cc -nostartfiles -no-pie OBJECT LINK... -o OUTPUT`. `LINK` has
   each `-lNAME` and `-LDIR` and all the arguments after `--`, in the order of
   the command line. The stdout of `cc` goes to stderr.
4. It removes its files and its directory, also when a step fails.

The driver writes nothing to stdout. It runs the tools with `fork` and
`execve`, not with a shell, so spaces and shell characters in arguments are
literal. It searches `fasm` and `cc` in `PATH` like `execvp`: an empty entry
is the current directory, and `/bin:/usr/bin` is used when `PATH` is not set.

The exit status is the status of the first tool that fails, `128 + N` when
signal `N` stops a tool, and `127` when a tool is not found or cannot run.
When an earlier phase fails, no tool runs.

`-l`, `-p`, `-P` and `-t` stop after a phase and write no file. `-o` cannot
be used with them. `-lNAME`, `-LDIR` and `--` are only for the executable,
so they cannot be used with these flags or with `-a`. `-l` without a name is
the lexer flag.

The output (the executable or the assembly file) must not be a source file
of the program (the main file or an import). For the assembly file the check
uses the open file before it is truncated. The driver does not clean up when
a signal stops `slc`.

`make examples` builds the examples with the driver. The Makefile gives the
`pkg-config` flags of raylib to the raylib example only.

## Packages

The default package has `bin/slc`, `bin/stackls` and the libraries in `lib`.
The packages `.#slc` and `.#stackls` have only one tool. `make` writes
`out/slc` and `out/stackls`. `make install` installs both tools and the
libraries into `PREFIX`.
