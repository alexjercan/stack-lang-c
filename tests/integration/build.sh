#!/usr/bin/env bash
# The build driver and the output modes of slc.
# Usage: build.sh <absolute-compiler-path>
# Needs fasm, cc, ar and a C compiler (CC, default cc) in PATH.

# shellcheck source-path=SCRIPTDIR source=utils.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/utils.sh" "$@"

STACK_HOME=$(cd -- "$here/../.." && pwd) || exit 1
export STACK_HOME
cc=${CC:-cc}
bash=$(command -v bash) || exit 1
real_fasm=$(command -v fasm) || exit 1
real_cc=$(command -v cc) || exit 1

# The driver makes its directory in TMPDIR. It must be empty after each build.
temp="$tmp/temp dir"
work="$tmp/work dir"
mkdir "$temp" || exit 1
cp -R "$here/fixtures/build" "$work" || exit 1
cd "$work" || exit 1

umask 022
rm=$(command -v rm) || exit 1
build() {
    local status left
    TMPDIR=$temp "$slc" "$@"
    status=$?
    shopt -s nullglob dotglob
    left=("$temp"/*)
    shopt -u nullglob dotglob
    [[ ${#left[@]} -eq 0 ]] || { echo "temporary files left: ${left[*]}" >&2; "$rm" -rf "${left[@]}"; return 99; }
    return "$status"
}
# with_path PATH CMD...: run CMD with PATH. CMD can be a function.
with_path() { local PATH=$1; export PATH; "${@:2}"; }
quiet() { "$@" 2> /dev/null; }
# expect TEXT...: write the lines into the file $tmp/expect.
expect() { printf '%s\n' "$@" > "$tmp/expect"; }
# tool DIR NAME BODY: make an executable script DIR/NAME.
tool() { mkdir -p "$1" && printf '#!%s\n%s\n' "$bash" "$3" > "$1/$2" && chmod +x "$1/$2"; }
absent() { [[ ! -e $1 ]]; }
run() { ./"$1"; }

# Normal builds. Nothing goes to stdout of slc.
expect hello
cp "$tmp/expect" "$tmp/expect-hello" || exit 1
check "typecheck" 0 "$empty" "$empty" build -t hello.stack
check "typecheck no output" 0 "$empty" "$empty" absent a.out
check "build" 0 "$empty" "$empty" build hello.stack -o hello
check "build run" 0 "$tmp/expect-hello" "$empty" run hello
check "default output" 0 "$empty" "$empty" build hello.stack
check "default output run" 0 "$tmp/expect-hello" "$empty" run a.out
check "stdin" 0 "$empty" "$empty" build -o stdin < hello.stack
check "stdin run" 0 "$tmp/expect-hello" "$empty" run stdin
check "replace output" 0 "$empty" "$empty" build hello.stack -o hello
check "-l is the lexer" 0 "$empty" "$empty" cmp <("$slc" --lexer hello.stack) <("$slc" -l hello.stack)
expect 'I/O error: cannot open source file "--build"'
check "no --build flag" 1 "$empty" "$tmp/expect" build --build

# Assembly to stdout or to a file. It needs no tools.
mkdir "$tmp/none" || exit 1
"$slc" -a hello.stack > "$tmp/hello.asm" || exit 1
check "assembly stdout" 0 "$tmp/hello.asm" "$empty" with_path "$tmp/none" build --assembler hello.stack
check "assembly file" 0 "$empty" "$empty" with_path "$tmp/none" build -a hello.stack -o 'h e.asm'
check "assembly file content" 0 "$empty" "$empty" cmp "$tmp/hello.asm" 'h e.asm'
cat "$tmp/hello.asm" "$tmp/hello.asm" > long.asm || exit 1
check "assembly file truncated" 0 "$empty" "$empty" build -o long.asm -a hello.stack
check "assembly file truncated content" 0 "$empty" "$empty" cmp "$tmp/hello.asm" long.asm
expect 'I/O error: cannot open output file "no dir/x.asm"'
check "assembly file missing directory" 1 "$empty" "$tmp/expect" build -a hello.stack -o 'no dir/x.asm'
expect 'I/O error: cannot write assembly output'
check "assembly file full" 1 "$empty" "$tmp/expect" build -a hello.stack -o /dev/full

# Arguments are literal: no shell runs.
src='s p;$(touch pwned)`touch pwned`*.stack'
out='o u|t;$(touch pwned)*'
cp hello.stack "$src" || exit 1
check "metacharacters" 0 "$empty" "$empty" build "$src" -o "$out"
check "metacharacters run" 0 "$tmp/expect-hello" "$empty" run "$out"
check "metacharacters no shell" 0 "$empty" "$empty" absent pwned

# C objects and libraries.
"$cc" -c -O2 -fno-stack-protector add.c -o 'add obj.o' || exit 1
"$cc" -c -O2 -fno-stack-protector twice.c -o twice.o || exit 1
mkdir 'lib dir' && ar rcs 'lib dir/libtwice.a' twice.o || exit 1
check "link" 0 "$empty" "$empty" build ffi.stack -o ffi '-Llib dir' -ltwice -- 'add obj.o' -Wl,-z,noexecstack
check "link run" 42 "$empty" "$empty" run ffi
check "missing object" 1 "$empty" "$empty" quiet build ffi.stack -o ffi2

# The exact arguments of the tools, and private temporary files.
fake=$tmp/fake
tool "$fake" fasm "printf '%s\n' \"\$@\" > '$tmp/fasm.args'; exec '$real_fasm' \"\$@\" > /dev/null"
tool "$fake" cc "printf '%s\n' \"\$@\" > '$tmp/cc.args'
d=\${3%/main.o}; stat -c '%a %F' \"\$d\" \"\$d/main.asm\" \"\$d/main.o\" >> '$tmp/cc.args'"
args() { sed "s|^$temp/slc-[0-9a-f]\{16\}/|TMP/|" "$tmp/$1.args"; }
check "fake tools" 0 "$empty" "$empty" with_path "$fake:$PATH" build ffi.stack -o 'x y' -lfoo -L/a -lbar -- 'a b.o' '-Wl,--defsym,$x=1' ';' '*' ''
expect TMP/main.asm TMP/main.o
check "fasm arguments" 0 "$tmp/expect" "$empty" args fasm
expect -nostartfiles -no-pie TMP/main.o -lfoo -L/a -lbar 'a b.o' '-Wl,--defsym,$x=1' ';' '*' '' -o 'x y' \
    '700 directory' '600 regular file' '644 regular file'
check "cc arguments" 0 "$tmp/expect" "$empty" args cc
check "fake output" 0 "$empty" "$empty" absent 'x y'

# Tool failures stop the build and give the status.
tool "$tmp/fasm3" fasm 'echo fasm says >&2; exit 3'
tool "$tmp/fasm3" cc "touch '$tmp/cc-ran'"
expect 'fasm says' 'Build error: `fasm` failed with exit status 3'
check "fasm status" 3 "$empty" "$tmp/expect" with_path "$tmp/fasm3:$PATH" build hello.stack
check "cc not run" 0 "$empty" "$empty" absent "$tmp/cc-ran"
tool "$tmp/cc5" cc 'echo cc stdout; echo cc stderr >&2; exit 5'
expect 'cc stdout' 'cc stderr' 'Build error: `cc` failed with exit status 5'
check "cc status" 5 "$empty" "$tmp/expect" with_path "$tmp/cc5:$PATH" build hello.stack
tool "$tmp/fasm-term" fasm 'kill -TERM $$'
expect 'Build error: `fasm` was killed by signal 15'
check "fasm signal" 143 "$empty" "$tmp/expect" with_path "$tmp/fasm-term:$PATH" build hello.stack
tool "$tmp/cc-kill" cc 'kill -KILL $$'
expect 'Build error: `cc` was killed by signal 9'
check "cc signal" 137 "$empty" "$tmp/expect" with_path "$tmp/cc-kill:$PATH" build hello.stack

# Missing tools. PATH search is like execvp: relative directories are relative
# to the current directory, an empty directory is the current directory, and
# entries that are not executable files are skipped.
mkdir "$tmp/only-fasm" && ln -s "$real_fasm" "$tmp/only-fasm/fasm" || exit 1
expect 'Build error: cannot find `fasm` in PATH'
check "missing fasm" 127 "$empty" "$tmp/expect" with_path "$tmp/none" build hello.stack
expect 'Build error: cannot find `cc` in PATH'
check "missing cc" 127 "$empty" "$tmp/expect" with_path "$tmp/only-fasm" build hello.stack
touch=$(command -v touch) || exit 1
tool rel fasm "'$touch' '$tmp/rel-ran'; exec '$real_fasm' \"\$@\""
check "relative PATH entry" 0 "$empty" "$empty" with_path "rel:$PATH" build hello.stack -o rel-out
check "relative PATH entry run" 0 "$empty" "$empty" test -e "$tmp/rel-ran"
# This cc makes the file $tmp/dot-OUTPUT.
tool . cc "'$touch' '$tmp/dot-'\"\${@: -1}\""
check "leading empty PATH entry" 0 "$empty" "$empty" with_path ":$tmp/only-fasm" build hello.stack -o lead
check "leading empty PATH entry run" 0 "$empty" "$empty" test -e "$tmp/dot-lead"
check "trailing empty PATH entry" 0 "$empty" "$empty" with_path "$tmp/only-fasm:" build hello.stack -o trail
check "trailing empty PATH entry run" 0 "$empty" "$empty" test -e "$tmp/dot-trail"
expect 'Build error: cannot find `fasm` in PATH'
check "empty PATH" 127 "$empty" "$tmp/expect" with_path "" build hello.stack
rm cc || exit 1
mkdir "$tmp/noexec" && printf '#!%s\nexit 1\n' "$bash" > "$tmp/noexec/cc" || exit 1
check "not executable PATH entry" 0 "$empty" "$empty" with_path "$tmp/noexec:$PATH" build hello.stack -o skipped
check "not executable PATH entry run" 0 "$tmp/expect-hello" "$empty" run skipped
mkdir "$tmp/noexe" && printf 'not a program\n' > "$tmp/noexe/cc" && chmod +x "$tmp/noexe/cc" || exit 1
expect "cannot execute \`$tmp/noexe/cc\`" 'Build error: `cc` failed with exit status 127'
check "exec failure" 127 "$empty" "$tmp/expect" with_path "$tmp/noexe:$PATH" build hello.stack
closed_stderr() { build hello.stack -o closed 2>&-; }
check "redirect failure" 127 "$empty" "$empty" closed_stderr
check "redirect failure no output" 0 "$empty" "$empty" absent closed
expect 'Build error: cannot create a temporary directory'
check "missing TMPDIR" 1 "$empty" "$tmp/expect" env TMPDIR="$tmp/missing" "$slc" hello.stack

# Invalid command lines. They do not run tools or write files.
cli() { expect "CLI error: $1"; check "cli: ${*:2}" 1 "$empty" "$tmp/expect" build "${@:2}"; }
cli '`-o` needs a file name' hello.stack -o
cli '`-o` needs a file name' hello.stack -o ''
cli '`-o` is given more than once' -o a -o b hello.stack
cli '`-L` needs a directory: use -LDIR' -L lib hello.stack
cli '`-o` cannot be used with `-t, --typecheck`' -t hello.stack -o x
cli '`-o` cannot be used with `-l, --lexer`' -l -o x hello.stack
cli '`-o` cannot be used with `-P, --preprocessor`' -P -a hello.stack -o x
cli '`-lNAME`, `-LDIR` and `--` cannot be used with `-a, --assembler`' -a -lm hello.stack
cli '`-lNAME`, `-LDIR` and `--` cannot be used with `-a, --assembler`' -L. -a hello.stack -o x
cli '`-lNAME`, `-LDIR` and `--` cannot be used with `-a, --assembler`' -a hello.stack --
cli '`-lNAME`, `-LDIR` and `--` cannot be used with `-p, --parser`' -p hello.stack -- x.o
check "cli no output" 0 "$empty" "$empty" absent x
"$slc" --typecheck bad.stack 2> "$tmp/bad.err"
check "compile error" 1 "$empty" "$tmp/bad.err" build bad.stack -o bad
check "compile error output" 0 "$empty" "$empty" absent bad

# The output must not be an input file. Also for the assembly file, which is
# checked before it is truncated.
ln -s hello.stack link.stack && ln hello.stack hard.stack || exit 1
for name in hello.stack ./hello.stack link.stack hard.stack; do
    expect "I/O error: output \`$name\` is an input file"
    check "output $name" 1 "$empty" "$tmp/expect" build hello.stack -o "$name"
    check "assembly output $name" 1 "$empty" "$tmp/expect" build -a hello.stack -o "$name"
done
expect 'I/O error: output `mod.stack` is an input file'
check "output import" 1 "$empty" "$tmp/expect" build main.stack -o mod.stack
check "assembly output import" 1 "$empty" "$tmp/expect" build -a main.stack -o mod.stack
expect 'I/O error: output `hello.stack` is an input file'
check "output stdin" 1 "$empty" "$tmp/expect" build -o hello.stack < hello.stack
check "assembly output stdin" 1 "$empty" "$tmp/expect" build -a -o hello.stack < hello.stack
for file in "$here"/fixtures/build/*; do
    check "unchanged ${file##*/}" 0 "$empty" "$empty" cmp "$file" "${file##*/}"
done
