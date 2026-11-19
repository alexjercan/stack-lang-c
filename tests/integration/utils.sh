# Shared setup for integration suites. Usage: source utils.sh "$@"
# shellcheck shell=bash disable=SC2034  # Variables are used by the suites.
# Sets: slc (compiler), here (suite dir), refs, empty, tmp (removed on exit).

set -o nounset -o pipefail

if [[ $# -ne 1 || $1 != /* || ! -x $1 ]]; then
    echo "usage: $0 <absolute-compiler-path>" >&2
    exit 2
fi

slc=$1
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd) || exit 1
refs=$here/refs
empty=$refs/empty.ref

tmp=$(mktemp -d) || exit 1
trap 'rm -rf -- "$tmp"' EXIT
trap 'exit 1' HUP INT TERM

# check NAME STATUS STDOUT_REF STDERR_REF CMD...
# Run CMD and compare its exit status, stdout, and stderr. Exit on mismatch.
check() {
    local name=$1 want=$2 out_ref=$3 err_ref=$4 got
    shift 4
    "$@" > "$tmp/out" 2> "$tmp/err"
    got=$?
    if [[ $got -eq $want ]] && cmp -s "$out_ref" "$tmp/out" && cmp -s "$err_ref" "$tmp/err"; then
        return 0
    fi
    {
        echo "FAILED: $name (exit status $got, expected $want)"
        diff -u --label "expected stdout" --label "actual stdout" "$out_ref" "$tmp/out"
        diff -u --label "expected stderr" --label "actual stderr" "$err_ref" "$tmp/err"
    } >&2
    exit 1
}
