# Evaluates `nixvimModules.default` with Nixvim. Nothing is built.
#
# The result is the list of the cases that pass. A case that fails stops the
# evaluation with an error. Run it with `tests/nixvim/run.sh eval`.
{
  nixvim,
  stack,
  system ? builtins.currentSystem,
}: let
  inherit (nixvim.lib.nixvim) evalNixvim;
  inherit (nixvim.inputs.nixpkgs) lib;
  inherit (builtins) attrNames filter hasAttr length;

  pkg = stack.packages.${system}.default;
  serverOnly = stack.packages.${system}.stackls;
  # The package of a case that must not use the package. Evaluation fails if
  # the module reads it.
  noPackage = throw "the module read stack.lsp.package";

  eval = module:
    (evalNixvim {
      inherit system;
      modules = [stack.nixvimModules.default module];
    })
    .config;
  baseline = (evalNixvim {inherit system;}).config;

  hasStackls = c: c.lsp.servers ? stackls && c.lsp.servers.stackls.enable;
  contains = needle: lib.hasInfix (builtins.unsafeDiscardStringContext needle);
  sameAsBaseline = c:
    c.content
    == baseline.content
    && attrNames c.extraFiles == attrNames baseline.extraFiles
    && c.filetype == baseline.filetype
    && c.keymaps == baseline.keymaps
    && c.extraPackages == baseline.extraPackages
    && c.build.package.drvPath == baseline.build.package.drvPath;
  noKeymaps = c: c.keymaps == baseline.keymaps && c.lsp.keymaps == baseline.lsp.keymaps;
  defaultConfig = p: {
    cmd = ["${p}/bin/stackls"];
    filetypes = ["stack"];
    cmd_env.STACK_HOME = "${p}";
  };

  disabled = eval {stack.lsp.package = noPackage;};
  disabledWithConfig = eval {
    stack.lsp.package = noPackage;
    stack.lsp.config.root_markers = [".git"];
  };
  syntaxOnly = eval {
    stack.syntax.enable = true;
    stack.lsp.package = noPackage;
  };
  lspDefaults = eval {stack.lsp.enable = true;};
  # The example in the README.
  example = eval {
    stack.syntax.enable = true;
    stack.lsp.enable = true;
    stack.lsp.config.root_markers = [".git"];
  };
  merged = eval {
    stack.lsp.enable = true;
    stack.lsp.config = {
      root_markers = [".git"];
      cmd_env.STACK_LOG = "1";
    };
  };
  replaced = eval {
    stack.lsp.enable = true;
    stack.lsp.config = {
      cmd = ["/opt/stack/out/stackls"];
      filetypes = ["stack" "stacklang"];
      cmd_env.STACK_HOME = "/opt/stack";
    };
  };
  serverPackage = eval {
    stack.lsp.enable = true;
    stack.lsp.package = serverOnly;
    stack.lsp.config.cmd_env.STACK_HOME = "${pkg}";
  };
  userFiletype = eval {
    stack.syntax.enable = true;
    filetype.extension.stack = "other";
  };
  directServer = eval {
    stack.lsp.enable = true;
    lsp.servers.stackls.config.root_markers = [".git"];
  };

  cases = {
    "disabled: same config as without the module" = sameAsBaseline disabled;
    "disabled: stack.lsp.config has no effect" = sameAsBaseline disabledWithConfig;

    "syntax: syntax/stack.vim is the bundled file" =
      toString syntaxOnly.extraFiles."syntax/stack.vim".source
      == "${stack.outPath}/editors/neovim/syntax/stack.vim";
    "syntax: *.stack has the stack filetype" = syntaxOnly.filetype.extension.stack == "stack";
    "syntax: no language server" = !(hasStackls syntaxOnly) && !(contains "stackls" syntaxOnly.content);
    "syntax: no keymaps" = noKeymaps syntaxOnly;
    "syntax: user filetype wins" = userFiletype.filetype.extension.stack == "other";

    "lsp: default client config" = lspDefaults.lsp.servers.stackls.config == defaultConfig pkg;
    "lsp: default package is packages.default" = lspDefaults.stack.lsp.package.outPath == pkg.outPath;
    "lsp: init.lua configures and enables stackls" =
      contains ''vim.lsp.config("stackls"'' lspDefaults.content
      && contains ''vim.lsp.enable("stackls")'' lspDefaults.content
      && contains "${pkg}/bin/stackls" lspDefaults.content;
    "lsp: no syntax file" = !(hasAttr "syntax/stack.vim" lspDefaults.extraFiles);
    "lsp: *.stack has the stack filetype" = lspDefaults.filetype.extension.stack == "stack";
    "lsp: no keymaps" = noKeymaps lspDefaults;
    "lsp: server is not put on PATH" = lspDefaults.extraPackages == baseline.extraPackages;

    "example: syntax, lsp and root_markers" =
      hasAttr "syntax/stack.vim" example.extraFiles
      && example.lsp.servers.stackls.config == defaultConfig pkg // {root_markers = [".git"];};

    "config: user keys merge with the defaults" =
      merged.lsp.servers.stackls.config
      == {
        cmd = ["${pkg}/bin/stackls"];
        filetypes = ["stack"];
        cmd_env = {
          STACK_HOME = "${pkg}";
          STACK_LOG = "1";
        };
        root_markers = [".git"];
      };
    "config: user values replace the defaults" =
      replaced.lsp.servers.stackls.config
      == {
        cmd = ["/opt/stack/out/stackls"];
        filetypes = ["stack" "stacklang"];
        cmd_env.STACK_HOME = "/opt/stack";
      };
    "config: lsp.servers.stackls.config also merges" =
      directServer.lsp.servers.stackls.config == defaultConfig pkg // {root_markers = [".git"];};

    "package: server-only package with STACK_HOME" =
      serverPackage.lsp.servers.stackls.config
      == {
        cmd = ["${serverOnly}/bin/stackls"];
        filetypes = ["stack"];
        cmd_env.STACK_HOME = "${pkg}";
      };
  };

  failed = filter (name: !cases.${name}) (attrNames cases);
in
  if failed == []
  then attrNames cases
  else throw "nixvim eval: ${toString (length failed)} failed:\n${lib.concatStringsSep "\n" failed}"
