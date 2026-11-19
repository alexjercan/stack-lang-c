# Nixvim module for Stack. The flake exports it as `nixvimModules.default`.
#
# The module does not depend on a Nixvim input. It uses the `lib`, `pkgs` and
# options of the Nixvim configuration that imports it.
{self}: {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) literalExpression mkDefault mkEnableOption mkIf mkMerge mkOption types;

  cfg = config.stack;
  system = pkgs.stdenv.hostPlatform.system;
in {
  options.stack = {
    syntax.enable = mkEnableOption "the Stack syntax file (`syntax/stack.vim`) and the `stack` filetype for `*.stack`";

    lsp = {
      enable = mkEnableOption "the `stackls` language server for the `stack` filetype (Neovim 0.11 or later)";

      package = mkOption {
        type = types.package;
        default = self.packages.${system}.default;
        defaultText = literalExpression "stack-lang-c.packages.\${system}.default";
        description = ''
          The package that gives `bin/stackls`. The client starts this
          executable by its absolute path, and `STACK_HOME` is the root of
          the package, so `@import` finds the libraries in `lib/` of the
          same package.

          The default package has `slc`, `stackls` and the libraries. A
          package that has only the server, for example
          `stack-lang-c.packages.''${system}.stackls`, has no `lib/`. With
          such a package, set `stack.lsp.config.cmd_env.STACK_HOME` to a
          directory that has `lib/`.
        '';
      };

      config = mkOption {
        type = types.attrsOf types.anything;
        default = {};
        example = literalExpression ''
          {
            root_markers = [".git"];
          }
        '';
        description = ''
          Client options for `vim.lsp.config("stackls", ...)`. See
          `:h vim.lsp.Config`. These values are merged with the defaults:
          `cmd`, `filetypes` and `cmd_env.STACK_HOME`. A value set here
          replaces the default value of the same key. Other keys of
          `cmd_env` are added to `STACK_HOME`.

          These options configure the client only. `stackls` supports only
          hover, and it does not read `settings` or `init_options`.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.syntax.enable || cfg.lsp.enable) {
      filetype.extension.stack = mkDefault "stack";
    })

    (mkIf cfg.syntax.enable {
      extraFiles."syntax/stack.vim".source = ./syntax/stack.vim;
    })

    (mkIf cfg.lsp.enable {
      lsp.servers.stackls = {
        enable = true;
        config = mkMerge [
          {
            cmd = mkDefault [(lib.getExe' cfg.lsp.package "stackls")];
            filetypes = mkDefault ["stack"];
            cmd_env.STACK_HOME = mkDefault "${cfg.lsp.package}";
          }
          cfg.lsp.config
        ];
      };
    })
  ];
}
