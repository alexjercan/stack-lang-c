#!/usr/bin/env bash
# The modules of the standard library. Each fixture imports one module (or the
# stdlib umbrella with modules) and runs. STACK_HOME is a copy of lib only, and
# the work directory has no module names, so a missing import fails.
# Usage: stdlib.sh <absolute-compiler-path>. Needs fasm and ld in PATH.

# shellcheck source-path=SCRIPTDIR source=utils.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/utils.sh" "$@"

work=$tmp/work
cp -R "$here/fixtures/stdlib" "$work" || exit 1
mkdir "$work/home" || exit 1
cp -R "$here/../../lib" "$work/home/lib" || exit 1
cd "$work" || exit 1
export STACK_HOME=home
ref=$refs/stdlib

# run NAME: build NAME.stack with fasm and ld, then run it.
run() {
    "$slc" --assembler "$1.stack" > "$1.asm" && fasm "$1.asm" "$1.o" > /dev/null &&
        ld "$1.o" -o "$1" && printf abc | STDLIB_TEST=a=b TMPDIR=$work ./"$1"
}

for name in linux core byte ptr string int array cstr error os; do
    check "use-$name" 0 "$empty" "$empty" run "use-$name"
done
check "use-io" 0 "$ref/io.out.ref" "$empty" run use-io
check "use-panic" 1 "$ref/panic.out.ref" "$ref/panic.err.ref" run use-panic
check "use-stdio" 0 "$ref/stdio.out.ref" "$ref/stdio.err.ref" run use-stdio
check "use-process" 7 "$empty" "$empty" run use-process
check "use-all" 0 "$ref/all.out.ref" "$empty" run use-all
check "missing import" 1 "$empty" "$ref/missing.err.ref" "$slc" --typecheck missing.stack
check "error.unwrap location" 1 "$empty" "$ref/error-unwrap.err.ref" run error-unwrap
