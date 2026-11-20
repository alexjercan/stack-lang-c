# Development

```fish
nix develop --command bash scripts/checker.sh --all
```

Run one stage with `--lexer`, `--parser`, `--preprocessor`, `--typecheck`,
`--io`, `--import`, or `--runtime`. Add `--no-build` to use the existing `out/slc`.

Runtime tests check that generated assembly contains only printable ASCII.

```fish
nix develop --command bash scripts/checker.sh --preprocessor --no-build -v
nix develop --command make test-lsp
nix flake check
```

Run the tests of the Nixvim module. `eval` evaluates the module and builds
nothing. `headless` builds Neovim and checks filetype, syntax, LSP and hover.
`NIXVIM` selects the Nixvim flake. Use the revision of your configuration:

```fish
env NIXVIM=github:nix-community/nixvim/<rev> tests/nixvim/run.sh eval
env NIXVIM=github:nix-community/nixvim/<rev> tests/nixvim/run.sh headless
```

The tests use `path:` of the repository, so untracked files are included.
Set `STACK` to use another reference of this flake.

## Packages

The default package has `bin/slc`, `bin/stackls` and the libraries in `lib`.
The packages `.#slc` and `.#stackls` have only one tool. `make` writes
`out/slc` and `out/stackls`. `make install` installs both tools and the
libraries into `PREFIX`.
