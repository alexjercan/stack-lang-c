#!/usr/bin/env bash
# Calls from Stack to C through extern. Usage: ffi.sh <absolute-compiler-path>
# Needs fasm, ld and a C compiler (CC, default cc).

# shellcheck source-path=SCRIPTDIR source=utils.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/utils.sh" "$@"

cc=${CC:-cc}
run_main() {
    "$slc" --assembler main.stack > main.asm &&
        fasm main.asm main.o > /dev/null &&
        ld -z noexecstack main.o ffi.o -o main &&
        ./main
}
assemble() { "$slc" --assembler "$1.stack" > "$1.asm" && fasm "$1.asm" "$1.o" > /dev/null; }

cp -R "$here/fixtures/ffi" "$tmp/work" || exit 1
cd "$tmp/work" || exit 1
"$cc" -c -O2 -fno-stack-protector ffi.c -o ffi.o || exit 1

ref=$refs/ffi
check "C calls" 0 "$empty" "$empty" run_main
check "one extrn per symbol" 0 "$ref/extrn.ref" "$empty" grep -c "^extrn 'add' as " main.asm
check "other types --typecheck" 0 "$empty" "$empty" "$slc" --typecheck types.stack
check "other types --assembler" 0 "$empty" "$empty" assemble types
check "bad externs --typecheck" 0 "$empty" "$empty" "$slc" --typecheck bad.stack
check "bad externs --assembler" 1 "$empty" "$ref/bad.err.ref" "$slc" --assembler bad.stack
check "bad externs default" 1 "$empty" "$ref/bad.err.ref" "$slc" bad.stack
