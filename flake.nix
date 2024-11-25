{
  description = "A basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: (
    flake-utils.lib.eachDefaultSystem
    (system: let
      pkgs = import nixpkgs {
        inherit system;

        config = {
          allowUnfree = true;
        };
      };
    in {
      packages.default = pkgs.stdenv.mkDerivation {
        pname = "slc";
        version = "0.0.1";

        makeFlags = ["PREFIX=$(out)"];

        nativeBuildInputs = with pkgs; [
          clang
          fasm
        ];

        src = ./.;
      };
      packages.checker = pkgs.writeShellApplication {
        name = "checker";
        text =
          /*
          bash
          */
          ''
            set +o errexit
            set +o pipefail

            MEMCHECKER=valgrind
            MAIN=main
            TESTS_DIR=tests
            TOTAL_TESTS=0
            PASSED_TESTS=0
            EXT=sl

            ARG1="all"
            MEMCHECK=0

            analyzer() {
                if [ "$#" -ne 3 ]; then
                    echo "Usage: $0 <tests_dir> <exec_arg> <memcheck>"
                    exit 1
                fi

                tests_dir=$TESTS_DIR/$1
                exec_arg=$2
                memcheck=$3

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

                    if  ./"$MAIN" "$exec_arg" "$file_path" 2>&1 | diff - "$ref_path" > /dev/null 2>&1 && [ "$memcheck_result" -eq 1 ]; then
                        echo -e "\e[32mPASSED\e[0m"
                        passed=$((passed + 1))
                    else
                        echo -e "\e[31mFAILED\e[0m"
                    fi
                done

                echo "Passed $passed/$total tests"
                TOTAL_TESTS=$((TOTAL_TESTS + total))
                PASSED_TESTS=$((PASSED_TESTS + passed))
            }

            usage() {
                echo "Usage: $0 [--all | --lexer | --parser] [--memcheck] [--main <main>] [-h | --help]"
                echo
                echo "Options:"
                echo "  --all           Enable 'all' mode."
                echo "  --lexer         Enable 'lexer' mode."
                echo "  --parser        Enable 'parser' mode."
                echo "  --memcheck      Enable memory check."
                echo "  --main <main>   Switch to a different main. Default is 'main'"
                echo "  -h, --help      Show this help message and exit."
            }

            main() {
                make clean && make

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
                        --memcheck)
                            MEMCHECK=1
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

                if [[ "$ARG1" == "lexer" || "$ARG1" == "all" ]]; then
                    echo -e "\e[33mTesting the lexical analyzer\e[0m"
                    analyzer "01-lexer" "--lexer" "$MEMCHECK"
                fi

                if [[ "$ARG1" == "parser" || "$ARG1" == "all" ]]; then
                    echo -e "\e[33mTesting the parser\e[0m"
                    analyzer "02-parser" "--parser" "$MEMCHECK"
                fi

                if [[ "$ARG1" == "typecheck" || "$ARG1" == "all" ]]; then
                    echo -e "\e[33mTesting the parser\e[0m"
                    analyzer "03-typecheck" "--typecheck" "$MEMCHECK"
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
          '';
      };
      devShells.default =
        pkgs.mkShell
        {
          name = "env-shell";

          nativeBuildInputs = with pkgs; [
            clang
            fasm
          ];
        };
    })
  );
}
