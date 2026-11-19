# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

### Added

- @alexjercan Added relative import paths
- @alexjercan Added the `stackls` language server
- @alexjercan Added hover to `stackls`
- @alexjercan Added the Nixvim module `nixvimModules.default` for syntax and `stackls`

### Changed

- @alexjercan Changed imports to use strings instead of names
- @alexjercan Moved `stack.vim` to `editors/neovim/syntax/stack.vim`

### Fixed

- @alexjercan The syntax file sets `b:current_syntax` and does not highlight keywords inside names

## [1.1.0] - 2024-12-29

### Added

- @alexjercan Implement Match Statement
- @alexjercan Implement While Statement

### Changed

- @alexjercan Typecheck expressions continues even on inference failure

### Fixed

- @alexjercan Preprocessor checks duplicate imports

## [1.0.0] - 2024-11-27

### Added

- @alexjercan Implement Stack Compiler in Stack

### Removed

- @alexjercan The C implementation of the Compiler

[unreleased]: https://github.com/alexjercan/stack-lang-c/compare/v1.1.0...HEAD
[1.1.0]: https://github.com/alexjercan/stack-lang-c/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/alexjercan/stack-lang-c/releases/tag/v1.0.0
