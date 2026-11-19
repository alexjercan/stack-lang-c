#!/usr/bin/env bash
# Source and output I/O errors. Usage: io.sh <absolute-compiler-path>

# shellcheck source-path=SCRIPTDIR source=utils.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/utils.sh" "$@"

to_full() { "$@" > /dev/full; }

cp -R "$here/fixtures/io" "$tmp/work" || exit 1
mkdir "$tmp/work/directory.stack" "$tmp/work/lib.stack" || exit 1
cd "$tmp/work" || exit 1

err=$refs/io
check "missing source" 1 "$empty" "$err/missing.err.ref" "$slc" --typecheck missing.stack
check "directory source" 1 "$empty" "$err/directory.err.ref" "$slc" --typecheck directory.stack
check "valid stdin" 0 "$empty" "$empty" "$slc" --typecheck < valid.stack
check "unreadable stdin" 1 "$empty" "$err/unreadable-stdin.err.ref" "$slc" --typecheck 0> /dev/null
check "closed stdin" 1 "$empty" "$err/closed-stdin.err.ref" "$slc" --typecheck 0<&-
check "closed stdin with file" 0 "$empty" "$empty" "$slc" --typecheck valid.stack 0<&-
check "directory import" 1 "$empty" "$err/directory-import.err.ref" "$slc" --typecheck import.stack
check "full output" 1 "$empty" "$err/full-output.err.ref" to_full "$slc" --assembler valid.stack
