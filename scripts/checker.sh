#!/usr/bin/env bash

set +o errexit

MAIN=out/slc
MODE=all
BUILD=1
MEMCHECK=0
VERBOSE=0
TOTAL=0
PASSED=0
shopt -s nullglob

usage() {
    echo "Usage: $0 [--all|--lexer|--parser|--preprocessor|--typecheck|--io|--import|--ffi|--runtime]"
    echo "       [--no-build] [--memcheck] [--main PATH] [-v|--verbose] [-h|--help]"
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --all|--lexer|--parser|--preprocessor|--typecheck|--io|--import|--ffi|--runtime)
            MODE=${1#--}; shift ;;
        --no-build) BUILD=0; shift ;;
        --memcheck) MEMCHECK=1; shift ;;
        -v|--verbose) VERBOSE=1; shift ;;
        --main)
            [[ $# -ge 2 && -n "$2" ]] || { usage; exit 1; }
            MAIN=$2; shift 2 ;;
        -h|--help) usage; exit 0 ;;
        *) usage; exit 1 ;;
    esac
done

if [[ "$BUILD" -eq 1 ]]; then
    make clean && make || exit 1
fi
MAIN=$(realpath -e "$MAIN") || exit 1
[[ -x "$MAIN" ]] || exit 1
WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

compiler() {
    local command=("$MAIN")
    if [[ "$MEMCHECK" -eq 1 ]]; then
        command=(valgrind --leak-check=full --errors-for-leak-kinds=all
            --error-exitcode=42 --log-file="$WORK/memcheck" "$MAIN")
    fi
    timeout 30 "${command[@]}" "$@"
}

check() {
    local name=$1 ref=$2 expected=$3 status actual=$WORK/out other=$WORK/err
    shift 3
    TOTAL=$((TOTAL + 1))
    printf 'Testing %s ... ' "$name"
    : > "$WORK/diff"
    : > "$WORK/build.log"
    : > "$WORK/memcheck"
    "$@" > "$WORK/out" 2> "$WORK/err"
    status=$?
    if [[ "$expected" -ne 0 ]]; then
        actual=$WORK/err
        other=$WORK/out
    fi
    if [[ "$status" -eq "$expected" && ! -s "$other" ]] &&
        diff -u "$ref" "$actual" > "$WORK/diff"; then
        printf '\033[32mPASSED\033[0m\n'
        PASSED=$((PASSED + 1))
    else
        printf '\033[31mFAILED\033[0m (exit %s, expected %s)\n' "$status" "$expected"
        if [[ "$VERBOSE" -eq 1 ]]; then
            cat "$WORK/diff" "$WORK/out" "$WORK/err" "$WORK/build.log" "$WORK/memcheck"
        fi
    fi
}

stage() {
    local dir=$1 mode=$2 file status
    local files=("$dir"/*.stack)
    [[ ${#files[@]} -gt 0 ]] || { echo "No tests in $dir" >&2; exit 1; }
    printf '\033[33mTesting %s\033[0m\n' "$mode"
    for file in "${files[@]}"; do
        status=0
        [[ "$mode" == typecheck && -s "${file%.stack}.ref" ]] && status=1
        [[ -f "${file%.stack}.status" ]] && read -r status < "${file%.stack}.status"
        check "$file" "${file%.stack}.ref" "$status" compiler "--$mode" "$file"
        if [[ "$mode" == preprocessor && -f "${file%.stack}.status" ]]; then
            check "$file --typecheck" "${file%.stack}.ref" "$status" compiler --typecheck "$file"
            check "$file --assembler" "${file%.stack}.ref" "$status" compiler --assembler "$file"
        fi
    done
}

compile_run() {
    compiler --assembler "$1" > "$WORK/main.asm" || return
    if LC_ALL=C grep -q '[^ -~]' "$WORK/main.asm"; then
        echo 'Assembly contains bytes outside printable ASCII' >&2
        return 1
    fi
    fasm "$WORK/main.asm" "$WORK/main.o" > "$WORK/build.log" 2>&1 &&
        ld "$WORK/main.o" -o "$WORK/main" >> "$WORK/build.log" 2>&1 &&
        timeout 10 "$WORK/main"
}

if [[ "$MODE" == all || "$MODE" == lexer ]]; then
    stage tests/01-lexer lexer
fi
if [[ "$MODE" == all || "$MODE" == parser ]]; then
    stage tests/02-parser parser
fi
if [[ "$MODE" == all || "$MODE" == preprocessor ]]; then
    STACK_HOME=tests/05-preprocessor/home stage tests/05-preprocessor preprocessor
fi
if [[ "$MODE" == all || "$MODE" == typecheck ]]; then
    stage tests/03-typecheck typecheck
fi
if [[ "$MODE" == all || "$MODE" == io ]]; then
    check 'I/O integration suite' /dev/null 0 timeout 60 bash tests/integration/io.sh "$MAIN"
fi
if [[ "$MODE" == all || "$MODE" == import ]]; then
    check 'Import integration suite' /dev/null 0 timeout 60 bash tests/integration/imports.sh "$MAIN"
fi
if [[ "$MODE" == all || "$MODE" == ffi ]]; then
    check 'FFI integration suite' /dev/null 0 timeout 60 bash tests/integration/ffi.sh "$MAIN"
fi
if [[ "$MODE" == all || "$MODE" == runtime ]]; then
    for file in tests/04-runtime/*.stack; do
        [[ "${file##*/}" == utils.stack ]] && continue
        status=0
        ref=/dev/null
        [[ -f "${file%.stack}.ref" ]] && ref=${file%.stack}.ref
        [[ -f "${file%.stack}.status" ]] && read -r status < "${file%.stack}.status"
        check "$file" "$ref" "$status" compile_run "$file"
    done
fi

if [[ "$PASSED" -eq "$TOTAL" ]]; then
    printf '\033[32mPassed %s/%s tests\033[0m\n' "$PASSED" "$TOTAL"
else
    printf '\033[31mPassed %s/%s tests\033[0m\n' "$PASSED" "$TOTAL"
    exit 1
fi
