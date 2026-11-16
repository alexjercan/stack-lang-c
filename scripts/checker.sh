#!/usr/bin/env bash

set +o errexit
set +o pipefail

MEMCHECKER=valgrind
MAIN=out/slc
TESTS_DIR=tests
TOTAL_TESTS=0
PASSED_TESTS=0
EXT=stack

ARG1="all"
MEMCHECK=0
VERBOSE=0
BUILD=1

analyzer() {
    if [ "$#" -ne 4 ]; then
        echo "Usage: $0 <tests_dir> <exec_arg> <memcheck> <verbose>"
        exit 1
    fi

    tests_dir=$TESTS_DIR/$1
    exec_arg=$2
    memcheck=$3
    verbose=$4

    echo "Running tests for $1"

    passed=0
    total=0
    for file_path in "$tests_dir"/*."$EXT"; do
        total=$((total + 1))

        ref_path="$tests_dir"/$(basename "$file_path" ."$EXT").ref

        file_name=$(basename "$file_path")
        echo -en "Testing $file_name ... "

        memcheck_result=1
        if [[ "$memcheck" -eq 1 ]]; then
            "$MEMCHECKER" --leak-check=full --errors-for-leak-kinds=all --error-exitcode=42 ./"$MAIN" "$exec_arg" "$file_path" > /dev/null 2>&1
            if [ $? -eq 42 ]; then
                memcheck_result=0
            else
                memcheck_result=1
            fi
        fi

        expected_status=0
        # Typechecker refs contain errors; empty refs describe valid programs.
        if [[ "$exec_arg" == "--typecheck" && -s "$ref_path" ]]; then
            expected_status=1
        fi
        # Negative cases can specify their expected exit status.
        status_path="${ref_path%.ref}.status"
        if [[ -f "$status_path" ]]; then
            read -r expected_status < "$status_path"
        fi
        diff_output=$(
            {
                ./"$MAIN" "$exec_arg" "$file_path" 2>&1
                compiler_status=$?
                if [[ "$compiler_status" -ne "$expected_status" ]]; then
                    printf 'Unexpected compiler exit status: got %s, expected %s\n' "$compiler_status" "$expected_status"
                fi
            } | diff - "$ref_path"
        )
        test_status=$?
        if [ "$test_status" -eq 0 ] && [ "$memcheck_result" -eq 1 ]; then
            echo -e "\e[32mPASSED\e[0m"
            passed=$((passed + 1))
        else
            echo -e "\e[31mFAILED\e[0m"
            if [ -n "$diff_output" ] && [ "$verbose" -eq 1 ]; then
                echo "$diff_output"
            fi
        fi
    done

    echo "Passed $passed/$total tests"
    TOTAL_TESTS=$((TOTAL_TESTS + total))
    PASSED_TESTS=$((PASSED_TESTS + passed))
}

runtime_analyzer() {
    local work_dir
    work_dir=$(mktemp -d) || return 1
    local passed=0 total=0 file_path file_name

    echo "Running executable tests for 04-runtime"
    for file_path in "$TESTS_DIR"/04-runtime/*."$EXT"; do
        # Shared helpers are modules, not executable tests.
        [[ "$(basename "$file_path")" == "utils.$EXT" ]] && continue
        total=$((total + 1))
        file_name=$(basename "$file_path")
        echo -n "Testing $file_name ... "

        if timeout 30 "$MAIN" --assembler "$file_path" > "$work_dir/main.asm" 2> "$work_dir/log" &&
            fasm "$work_dir/main.asm" "$work_dir/main.o" >> "$work_dir/log" 2>&1 &&
            ld "$work_dir/main.o" -o "$work_dir/main" >> "$work_dir/log" 2>&1 &&
            timeout 10 "$work_dir/main" >> "$work_dir/log" 2>&1; then
            echo "PASSED"
            passed=$((passed + 1))
        else
            echo "FAILED (compiler, assembler, linker, or executable exited with $?)"
            if [[ "$VERBOSE" -eq 1 ]]; then
                while IFS= read -r line; do printf '%s\n' "$line"; done < "$work_dir/log"
            fi
        fi
    done

    rm -rf "$work_dir"
    echo "Passed $passed/$total executable tests"
    TOTAL_TESTS=$((TOTAL_TESTS + total))
    PASSED_TESTS=$((PASSED_TESTS + passed))
}

io_check() {
    local name=$1 expected_status=$2 expected_stderr=$3 out_path=$4 status
    shift 4
    IO_TOTAL=$((IO_TOTAL + 1))
    echo -n "Testing $name ... "

    "$@" > "$out_path" 2> "$IO_DIR/err"
    status=$?
    if [[ "$status" -eq "$expected_status" &&
        "$(< "$IO_DIR/err")" == "$expected_stderr" &&
        ! ( -f "$out_path" && -s "$out_path" && "$expected_status" -ne 0 ) ]]; then
        echo "PASSED"
        IO_PASSED=$((IO_PASSED + 1))
    else
        echo "FAILED (exit status $status, expected $expected_status)"
        if [[ "$VERBOSE" -eq 1 ]]; then
            while IFS= read -r line; do printf '%s\n' "$line"; done < "$IO_DIR/err"
        fi
    fi
}

io_analyzer() {
    IO_DIR=$(mktemp -d) || return 1
    IO_TOTAL=0
    IO_PASSED=0
    local out="$IO_DIR/out"

    printf 'func main () (int) in\n    0\nend\n' > "$IO_DIR/valid.$EXT"
    mkdir "$IO_DIR/directory.$EXT" "$IO_DIR/lib.$EXT"
    printf '@import "%s/lib"\n\nfunc main () (int) in\n    0\nend\n' "$IO_DIR" > "$IO_DIR/import.$EXT"

    echo "Running I/O tests"
    io_check "missing source" 1 "I/O error: cannot open source file \"$IO_DIR/missing.$EXT\"" "$out" \
        "$MAIN" --typecheck "$IO_DIR/missing.$EXT"
    io_check "directory source" 1 "I/O error: cannot read source file \"$IO_DIR/directory.$EXT\"" "$out" \
        "$MAIN" --typecheck "$IO_DIR/directory.$EXT"
    io_check "valid stdin" 0 "" "$out" \
        "$MAIN" --typecheck < "$IO_DIR/valid.$EXT"
    io_check "unreadable stdin" 1 "I/O error: cannot read standard input" "$out" \
        "$MAIN" --typecheck 0> /dev/null
    io_check "directory import" 1 "$IO_DIR/import.$EXT:1:9, Preprocessor error: cannot read import \"$IO_DIR/lib\"" "$out" \
        "$MAIN" --typecheck "$IO_DIR/import.$EXT"
    io_check "full output" 1 "I/O error: cannot write assembly output" /dev/full \
        "$MAIN" --assembler "$IO_DIR/valid.$EXT"

    rm -rf "$IO_DIR"
    echo "Passed $IO_PASSED/$IO_TOTAL I/O tests"
    TOTAL_TESTS=$((TOTAL_TESTS + IO_TOTAL))
    PASSED_TESTS=$((PASSED_TESTS + IO_PASSED))
}

usage() {
    echo "Usage: $0 [--all | --lexer | --parser | --preprocessor | --typecheck | --io | --runtime] [--no-build] [--memcheck] [--main <main>] [-h | --help]"
    echo
    echo "Options:"
    echo "  --all           Enable 'all' mode."
    echo "  --lexer         Enable 'lexer' mode."
    echo "  --parser        Enable 'parser' mode."
    echo "  --preprocessor  Enable 'preprocessor' mode."
    echo "  --typecheck     Enable 'typecheck' mode."
    echo "  --io            Enable 'io' mode."
    echo "  --runtime       Compile, assemble, link, and run executable tests."
    echo "  --no-build      Use the existing compiler without rebuilding."
    echo "  --memcheck      Enable memory check."
    echo "  -v, --verbose   Show more details in case of errors."
    echo "  --main <main>   Switch to a different main. Default is 'main'"
    echo "  -h, --help      Show this help message and exit."
}

main() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --all)
                ARG1="all"
                shift
                ;;
            --lexer)
                ARG1="lexer"
                shift
                ;;
            --parser)
                ARG1="parser"
                shift
                ;;
            --preprocessor)
                ARG1="preprocessor"
                shift
                ;;
            --typecheck)
                ARG1="typecheck"
                shift
                ;;
            --io)
                ARG1="io"
                shift
                ;;
            --runtime)
                ARG1="runtime"
                shift
                ;;
            --no-build)
                BUILD=0
                shift
                ;;
            --memcheck)
                MEMCHECK=1
                shift
                ;;
            -v|--verbose)
                VERBOSE=1
                shift
                ;;
            --main)
                if [ -z "$2" ]; then
                    echo "$1"
                    echo "$2"
                    usage
                    exit 1
                fi
                MAIN="$2"
                shift
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                usage
                exit 1
                ;;
        esac
    done

    if [[ "$BUILD" -eq 1 ]]; then
        make clean && make || return 1
    fi

    if [[ "$ARG1" == "lexer" || "$ARG1" == "all" ]]; then
        echo -e "\e[33mTesting the lexical analyzer\e[0m"
        analyzer "01-lexer" "--lexer" "$MEMCHECK" "$VERBOSE"
    fi

    if [[ "$ARG1" == "parser" || "$ARG1" == "all" ]]; then
        echo -e "\e[33mTesting the parser\e[0m"
        analyzer "02-parser" "--parser" "$MEMCHECK" "$VERBOSE"
    fi

    if [[ "$ARG1" == "preprocessor" || "$ARG1" == "all" ]]; then
        echo -e "\e[33mTesting the preprocessor\e[0m"
        # Fallback imports resolve to fixture modules, not the host STACK_HOME.
        STACK_HOME="$TESTS_DIR/05-preprocessor/home" analyzer "05-preprocessor" "--preprocessor" "$MEMCHECK" "$VERBOSE"
    fi

    if [[ "$ARG1" == "typecheck" || "$ARG1" == "all" ]]; then
        echo -e "\e[33mTesting the typechecker\e[0m"
        analyzer "03-typecheck" "--typecheck" "$MEMCHECK" "$VERBOSE"
    fi

    if [[ "$ARG1" == "io" || "$ARG1" == "all" ]]; then
        io_analyzer || return 1
    fi

    if [[ "$ARG1" == "runtime" || "$ARG1" == "all" ]]; then
        runtime_analyzer || return 1
    fi

    if [ $PASSED_TESTS -eq $TOTAL_TESTS ]; then
        echo -e "\e[32mAll tests passed\e[0m"
        exit 0
    else
        echo -e "\e[31mSome tests failed\e[0m"
        exit 1
    fi
}

if [[ $# -eq 1 && ($1 == "--help" || $1 == "-h") ]]; then
    usage
    exit 0
fi

main "$@"
