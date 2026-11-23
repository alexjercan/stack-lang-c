#!/usr/bin/env bash
# Tests nixvimModules.default with Nixvim.
#
#   tests/nixvim/run.sh [eval|headless|syntax|all]
#
#   eval      evaluate the module in some configurations; builds nothing
#   syntax    check syntax/stack.vim with the nvim of PATH (or NVIM_BIN);
#             builds nothing
#   headless  build Neovim with the module and check filetype, syntax and
#             hover with stackls; builds the default package
#
# NIXVIM is the Nixvim flake (default: github:nix-community/nixvim).
# STACK is this flake (default: path: of the repository, so new files that
# git does not track yet are included).
set -euo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "$here/../.." && pwd)
nixvim=${NIXVIM:-github:nix-community/nixvim}
stack=${STACK:-path:$repo}
mode=${1:-all}

nix_expr() {
  printf 'import %s/%s { nixvim = builtins.getFlake "%s"; stack = builtins.getFlake "%s"; }' \
    "$here" "$1" "$nixvim" "$stack"
}

run_eval() {
  nix eval --impure --json --expr "$(nix_expr eval.nix)"
  echo "nixvim eval: ok"
}

run_headless() {
  local nvim tmp
  nvim=$(nix build --impure --no-link --print-out-paths --expr "$(nix_expr nvim.nix)")
  tmp=$(mktemp -d)
  trap 'rm -rf "$tmp"' RETURN
  mkdir -p "$tmp/project"
  (
    cd "$tmp/project"
    XDG_CONFIG_HOME="$tmp/config" XDG_DATA_HOME="$tmp/data" \
      XDG_STATE_HOME="$tmp/state" XDG_CACHE_HOME="$tmp/cache" \
      timeout 60 "$nvim/bin/nvim" --headless -n -i NONE \
      -c "luafile $here/headless.lua"
  )
  echo "nixvim headless: ok"
}

run_syntax() {
  timeout 60 "${NVIM_BIN:-nvim}" --clean --headless -n -i NONE \
    --cmd "set runtimepath^=$repo/editors/neovim" \
    -c "luafile $here/syntax.lua"
  echo "syntax: ok"
}

case $mode in
eval) run_eval ;;
headless) run_headless ;;
syntax) run_syntax ;;
all)
  run_eval
  run_syntax
  run_headless
  ;;
*)
  echo "usage: $0 [eval|headless|syntax|all]" >&2
  exit 2
  ;;
esac
