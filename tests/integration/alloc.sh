#!/usr/bin/env bash
# The mmap allocator of the runtime. Usage: alloc.sh <absolute-compiler-path>
# Needs fasm, cc and a C compiler (CC, default cc) in PATH.

# shellcheck source-path=SCRIPTDIR source=utils.sh
source "$(dirname -- "${BASH_SOURCE[0]}")/utils.sh" "$@"

STACK_HOME=$(cd -- "$here/../.." && pwd) || exit 1
export STACK_HOME
cc=${CC:-cc}

cp -R "$here/fixtures/alloc" "$tmp/work" || exit 1
cd "$tmp/work" || exit 1
"$cc" -c -O2 -fno-stack-protector libc.c -o libc.o || exit 1
printf 'Runtime error: invalid free\n' > "$tmp/invalid"

# build NAME [CC-ARGUMENT...]: build NAME.stack into NAME.
build() { "$slc" "$1.stack" -o "$1" "${@:2}"; }
run() { timeout 60 ./"$1"; }

check "regions build" 0 "$empty" "$empty" build regions
check "regions" 0 "$empty" "$empty" run regions
check "tail build" 0 "$empty" "$empty" build tail
check "tail" 0 "$empty" "$empty" run tail
for name in free-cell free-argv free-region-header free-region-end free-forged; do
    check "$name build" 0 "$empty" "$empty" build "$name"
    check "$name" 1 "$empty" "$tmp/invalid" run "$name"
done

# libc malloc and the runtime use different memory.
check "libc build" 0 "$empty" "$empty" build libc -- libc.o
check "libc" 0 "$empty" "$empty" run libc
check "foreign build" 0 "$empty" "$empty" build foreign -- libc.o
check "foreign" 1 "$empty" "$tmp/invalid" run foreign
check "hole build" 0 "$empty" "$empty" build hole -- libc.o
check "hole" 1 "$empty" "$tmp/invalid" run hole
