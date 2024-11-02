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

            MAIN=main
            TESTS_DIR=tests
            TOTAL_TESTS=0
            PASSED_TESTS=0
            EXT=sl

            analyzer() {
                if [ "$#" -ne 2 ]; then
                    echo "Usage: $0 <tests_dir> <exec_arg>"
                    exit 1
                fi

                tests_dir=$TESTS_DIR/$1
                exec_arg=$2

                echo "Running tests for $1"

                passed=0
                total=0
                for file_path in "$tests_dir"/*."$EXT"; do
                    total=$((total + 1))

                    ref_path="$tests_dir"/$(basename "$file_path" ."$EXT").ref

                    file_name=$(basename "$file_path")
                    echo -en "Testing $file_name ... "

                    if ./"$MAIN" "$exec_arg" "$file_path" 2>&1 | diff - "$ref_path" > /dev/null 2>&1; then
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
                echo "Usage: $0 [--all | --lexer]"
            }

            main() {
                make clean && make

                if [[ $# -eq 0 ]]; then
                    ARG1="--all"
                else
                    ARG1=$1
                fi

                if [[ "$ARG1" == "--lexer" || "$ARG1" == "--all" ]]; then
                    echo -e "\e[33mTesting the lexical analyzer\e[0m"
                    analyzer "01-lexer" "--lexer"
                else
                    usage
                    exit 1
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
