#!/usr/bin/env bash
# Import identity across path aliases, links, copies, stdin, and STACK_HOME.
# Usage: imports.sh <absolute-compiler-path>
# Fixtures import "/proc/self/cwd/..." to get a stable absolute alias.

# shellcheck source-path=SCRIPTDIR source=utils.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/utils.sh" "$@"

piped() { cat "$1" | "${@:2}"; }
run_main() { "$slc" --assembler main.stack > m.asm && fasm m.asm m.o > /dev/null && ld m.o -o m && ./m; }

work=$tmp/work
cp -R "$here/fixtures/imports" "$work" || exit 1
cd "$work" || exit 1
mkdir sub || exit 1
ln -s mod.stack link.stack || exit 1
ln mod.stack hard.stack || exit 1
cp mod.stack copy.stack || exit 1
ln -s main.stack mainlink.stack || exit 1

ref=$refs/imports
check "aliases --preprocessor" 0 "$ref/main.pre.ref" "$empty" "$slc" --preprocessor main.stack
check "aliases --typecheck" 0 "$empty" "$empty" "$slc" --typecheck main.stack
check "aliases executable" 43 "$empty" "$empty" run_main
check "absolute main" 0 "$ref/main.pre.ref" "$empty" "$slc" --preprocessor "$work/main.stack"
check "symlink main" 0 "$ref/main.pre.ref" "$empty" "$slc" --preprocessor mainlink.stack
check "file stdin main" 0 "$ref/main.pre.ref" "$empty" "$slc" --preprocessor < main.stack
check "pipe stdin main" 1 "$empty" "$ref/pipe-main.err.ref" piped main.stack "$slc" --typecheck
check "fallback alias" 0 "$ref/fallback.pre.ref" "$empty" env STACK_HOME=home "$slc" --preprocessor fallback.stack
for mode in --preprocessor --typecheck --assembler; do
    check "copied file $mode" 1 "$empty" "$ref/copied.err.ref" "$slc" "$mode" copied.stack
    check "duplicate const $mode" 1 "$empty" "$ref/dconst.err.ref" "$slc" "$mode" dconst.stack
done
for mode in --typecheck --assembler; do
    check "duplicate func $mode" 1 "$empty" "$ref/dfunc.err.ref" "$slc" "$mode" dfunc.stack
    check "duplicate data $mode" 1 "$empty" "$ref/ddata.err.ref" "$slc" "$mode" ddata.stack
done
