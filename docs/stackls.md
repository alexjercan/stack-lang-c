# Language Server

`stackls` is a language server for `Stack`. It reads LSP messages on stdin,
writes them on stdout, and writes logs to stderr.

```console
stackls [-h | --help] [-v | --version] [--stdio]
```

The server supports hover, go to definition and completion. It advertises
`"hoverProvider": true`, `"definitionProvider": true`,
`"completionProvider": {"triggerCharacters": ["."]}` and full text
synchronization (`"textDocumentSync": {"openClose": true, "change": 1}`).
References and diagnostics are not supported.

The source is `stackls.stack`. It uses these libraries:

- `lib/rpc.stack`: message framing.
- `lib/lsp.stack`: JSON-RPC, the lifecycle, open documents and positions. It
  does not know `Stack`, and other programs can import it.
- `lib/json.stack`: JSON values.
- `lib/stack/service.stack` and `lib/stack/analysis.stack`: the `Stack`
  requests. They use the parts of the compiler: `lib/stack/lexer.stack`,
  `lib/stack/parser.stack`, `lib/stack/preprocessor.stack` and
  `lib/stack/typecheck.stack`. `stack.stack` uses the same files.

`tests/lsp/protocol.py` uses only the Python standard library. It acts as a
client: it starts one server per case, sends bytes, and checks the exit code
and every byte on stdout. The hover tests that import `stdlib` use
`../lib` from the directory of the server: the repository for `out/stackls`,
and the package for `result/bin/stackls`. You can run it on such a binary:

```fish
python3 tests/lsp/protocol.py result/bin/stackls
```

## Protocol

Framing:

- Header lines end with `\r\n`, and an empty line ends the header.
- Header names do not use case. `Content-Length` must be one decimal number
  of at most 64 MiB, and it must occur one time. Other valid headers, for
  example `Content-Type`, are ignored.
- The header can have at most 8 KiB.
- A bad header, or the end of input inside a message, stops the server with
  exit code 1 and a message on stderr. The end of input between messages
  stops the server with exit code 0.

JSON-RPC:

- A body that is not valid JSON gets error `-32700` with `"id": null`. The
  server then reads the next message.
- A message that is not a valid request or notification gets error `-32600`.
  `jsonrpc` must be `"2.0"`, `method` must be a string, `params` must be an
  object, an array or `null`, and `id` must be a string or an integer that
  fits in 64 bits. Ids are sent back as they are.
- A message without `method` and with `result` or `error` is a response. The
  server ignores it.
- Notifications get no response. Unknown notifications are ignored.

Lifecycle:

- Before `initialize`, requests get `-32002`.
- `initialize` accepts `params` that are `null`, missing or an object.
  `clientInfo` is optional. When it is present, it must be an object with a
  string `name` and an optional string `version`, or the request gets
  `-32602`. Other fields are ignored.
- After `initialize`, unknown requests get `-32601`, and `initialize` again
  gets `-32600`.
- `shutdown` gets `null`. After it, requests get `-32600`.
- `exit` stops the server with code 0 after `shutdown`, and 1 if there was no
  `shutdown`.

Documents:

- `textDocument/didOpen`, `textDocument/didChange` and
  `textDocument/didClose` keep the text and the version of each open
  document. A change must have the full text. A change with a `range` is
  ignored and logged. Notifications with bad `params` are ignored and logged.
- The version of an open document must increase. `didChange`, or `didOpen`
  of a document that is open, with the same or a lower version is ignored
  and logged.
- The text of an open document is used in place of the file on disk, also
  when another file imports it. A URI that is not a `file://` URI can be
  open, but other files cannot import it.

Hover (`textDocument/hover`):

- Positions use UTF-16 code units, and lines end with `\n`, `\r\n` or `\r`.
  `line` and `character` must be integers from 0 to 2147483647, or the
  request gets `-32602`. A character after the end of a line is the end of
  the line, and a line after the end of the document gets `null`.
- The result has the code of the definition and a short note, and the range
  of the name. The content is Markdown with a `stack` code block when the
  first known kind in `capabilities.textDocument.hover.contentFormat` is
  `markdown`, else plain text.
- These names get a result: builtin functions and data types, the `func`,
  `data`, `const` and `extern` definitions of the file and of its imports,
  the type names in signatures and fields, `T.&` and `T.*`, and the generated
  constants `T.init`, `T.sizeof`, `T.f`, `T.f.set` and `T.f.offset` with
  their stack effect. A `const` shows its original text. An `extern` shows
  its signature and its C symbol, for example
  `extern c.add (int, int) (int) "add"`.
- The `--` lines just above a `func`, `data`, `const` or `extern` are its
  description.
  It is raw Markdown, so write `array<...>` in backticks.
- A match binding and the names that it binds in a `func` show
  `name: type` from the type checker of `slc`. They get `null` when the
  checker gives no type: an error before the binding in the `func`, a
  binding in a `const`, or a name with two definitions.
- These positions get `null`: literals, keywords, comments, type variables,
  names that do not resolve, and documents that are not open. A bound name
  that is also a constant is the constant, as in `slc`.
- Bad `params` get `-32602`.
- Imports resolve as in `slc`: `name.stack` relative to the directory where
  the server runs, then `$STACK_HOME/lib/name.stack`.
- If a file has a syntax error, the definitions before the error are used.
  Names after the error get `null`. A missing or broken import keeps the
  definitions that were loaded before it.

Go to definition (`textDocument/definition`) has the same positions, params,
imports and errors as hover. The result is a `Location` with the range of the
name of the definition, or `null`:

- A `func`, `extern`, `data`, `const`, field or match binding is its own
  definition. A name uses the definition that `slc` uses: the data helpers,
  then the constants, then the builtins, then the first `func`, `extern` or
  `data`. An `extern` goes to its name in the `Stack` file, not to C code.
- A bound name goes to the binding of the innermost `match`. It does not
  need a type.
- `T.f`, `T.f.set` and `T.f.offset` go to the field `f`. `T.init`,
  `T.sizeof`, `T.&` and `T.*` go to the name of `T`.
- Builtins, special names, literals and names that do not resolve get `null`.
- The URI of an open document stays as the client sent it. Other files get a
  `file://` URI of the absolute path, with percent escapes. `..` parts stay
  as they are, because a directory before them can be a symbolic link.

Completion (`textDocument/completion`) has the same positions, params and
imports as hover. The result is a `CompletionList` that is complete:

- The items are the names that start with the text from the start of the name
  at the cursor to the cursor. The `textEdit` of an item replaces all of that
  name, also the part after the cursor. A name can have dots and operator
  characters, as in `ptr.+`.
- In a body: data helpers, constants, builtin functions, the match bindings
  in scope, and `func` and `extern` definitions with `T.&` and `T.*`. In a
  signature or a data field: data types. A label is there one time, from the definition that
  `slc` uses. `detail` is the signature or the type, when it is known.
- The scan of the tokens finds the bindings in scope, so code that is not
  complete works. The names of `func`, `data`, `const` and `extern`
  definitions after a syntax error are also there, without `detail`.
- No items in comments, strings, numbers, or the name of a new definition,
  field or binding. There are no keywords and no snippets.

## Known limitations

- Memory grows with each message. The `Stack` runtime puts each computed
  `int`, `bool` and `ptr` into a cell that it never frees (see
  [Runtime memory](runtime.md)). The server frees each JSON tree, frame buffer,
  document text and the lists of an analysis with `ptr.free`, but cells and
  syntax trees stay. A measurement gave about 150 KB for each small request.
- The first hover after a change analyses the document again. Later hovers
  use the same analysis until a document opens, changes or closes. The server
  keeps the parsed imports and parses an import again only when its text
  changes. Measurements on x86-64 Linux: the first hover in a file that
  imports `stdlib` adds about 50 MB, and each hover after a change about
  5 MB. For a file that imports the compiler (`stackls.stack`) the first hover
  adds about 620 MB and each hover after a change about 50 MB. Restart the
  server to give the memory back.
- The server reads an import that is not open from disk at each analysis.
  It does not watch files, so a change on disk is seen at the next analysis.
- Imports resolve from the directory where the editor starts the server.
- The server handles one message at a time. `$/cancelRequest` is ignored.
- If the client closes stdout, the write gets `SIGPIPE` and the server stops.
- The server runs only on x86-64 Linux, as the `Stack` compiler.

## Neovim

The flake exports a [Nixvim](https://github.com/nix-community/nixvim) module
as `nixvimModules.default`. The flake does not depend on Nixvim. The module
uses the `lib` and `pkgs` of the configuration that imports it. It needs
Neovim 0.11 or later, because it uses `vim.lsp.config` and `vim.lsp.enable`.
The syntax file is also in `editors/neovim/syntax/stack.vim`.

Add this repository to the inputs of your flake, and give the inputs to
Home Manager:

```nix
{
  inputs.stack-lang-c.url = "github:alexjercan/stack-lang-c";

  # home-manager.lib.homeManagerConfiguration {
  #   extraSpecialArgs = {inherit inputs;};
  #   ...
  # }
}
```

Then import the module in `programs.nixvim`:

```nix
{inputs, ...}: {
  programs.nixvim = {
    imports = [inputs.stack-lang-c.nixvimModules.default];
    stack.syntax.enable = true;
    stack.lsp.enable = true;
    stack.lsp.config = {root_markers = [".git"];};
  };
}
```

Options:

- `stack.syntax.enable` (default `false`): adds `syntax/stack.vim`.
- `stack.lsp.enable` (default `false`): configures and enables the client
  `stackls` with `vim.lsp.config` and `vim.lsp.enable`.
- `stack.lsp.package` (default: the `default` package of this flake): the
  package that gives `bin/stackls`.
- `stack.lsp.config` (default `{}`): client options for
  `vim.lsp.config("stackls", ...)`, see `:h vim.lsp.Config`.

Each of the two features also maps `*.stack` to the filetype `stack`. If
both are `false`, the module changes nothing.

The default client options are:

- `cmd`: the absolute path of `bin/stackls` in `stack.lsp.package`.
- `filetypes`: `["stack"]`.
- `cmd_env.STACK_HOME`: the root of `stack.lsp.package`. `@import` finds
  the libraries in `$STACK_HOME/lib`.

A key in `stack.lsp.config` replaces the default value of that key. Other
keys of `cmd_env` are added to `STACK_HOME`. `stack.lsp.config` configures
only the client. `stackls` supports only hover, and it does not read
`settings` or `init_options`.

The module adds no keymaps. Use your own keymap for hover, for example `K`
from `plugins.lsp.keymaps`.

`STACK_HOME` must have the libraries of the same version as the server. The
default package has `bin/slc`, `bin/stackls` and `lib`, so its root is
correct. The package `stackls` has only the server. If you use it, set
`STACK_HOME` to a directory that has `lib`:

```nix
{inputs, pkgs, ...}: let
  stack = inputs.stack-lang-c.packages.${pkgs.stdenv.hostPlatform.system};
in {
  programs.nixvim.stack.lsp = {
    enable = true;
    package = stack.stackls;
    config.cmd_env.STACK_HOME = "${stack.default}";
  };
}
```

To use a development build from `make`, set `cmd` and `STACK_HOME` to the
repository:

```nix
{
  programs.nixvim.stack.lsp = {
    enable = true;
    config = {
      cmd = ["/home/me/stack-lang-c/out/stackls"];
      cmd_env.STACK_HOME = "/home/me/stack-lang-c";
    };
  };
}
```

Neovim starts the server in its current directory. `@import` of a relative
path resolves from that directory (see [Known limitations](#known-limitations)).
Set `cmd_cwd` in `stack.lsp.config` to use another directory.
