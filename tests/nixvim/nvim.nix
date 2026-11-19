# Neovim from Nixvim with the README example. `run.sh headless` uses it.
{
  nixvim,
  stack,
  system ? builtins.currentSystem,
}:
nixvim.legacyPackages.${system}.makeNixvimWithModule {
  module = {
    imports = [stack.nixvimModules.default];
    stack.syntax.enable = true;
    stack.lsp.enable = true;
    stack.lsp.config.root_markers = [".git"];
  };
}
