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

usage() {
    echo "Usage: $0 [--all | --lexer | --parser | --typecheck | --runtime] [--no-build] [--memcheck] [--main <main>] [-h | --help]"
    echo
    echo "Options:"
    echo "  --all           Enable 'all' mode."
    echo "  --lexer         Enable 'lexer' mode."
    echo "  --parser        Enable 'parser' mode."
    echo "  --typecheck     Enable 'typecheck' mode."
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
            --typecheck)
                ARG1="typecheck"
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

    if [[ "$ARG1" == "typecheck" || "$ARG1" == "all" ]]; then
        echo -e "\e[33mTesting the typechecker\e[0m"
        analyzer "03-typecheck" "--typecheck" "$MEMCHECK" "$VERBOSE"
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
