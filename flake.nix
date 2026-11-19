{
  description = "A basic flake for Stack Lang";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];

      perSystem = {
        config,
        pkgs,
        system,
        ...
      }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        packages.slc = pkgs.stdenv.mkDerivation {
          pname = "slc";
          version = "1.1.0";

          makeFlags = ["PREFIX=$(out)"];
          dontBuild = true;
          installTargets = ["install-slc"];

          nativeBuildInputs = with pkgs; [
            fasm
          ];

          src = ./.;
        };
        packages.stackls = pkgs.stdenv.mkDerivation {
          pname = "stackls";
          version = "0.1.0";

          makeFlags = ["PREFIX=$(out)" "SLC=${config.packages.slc}/bin/slc"];
          dontBuild = true;
          installTargets = ["install-stackls"];

          nativeBuildInputs = with pkgs; [
            fasm
          ];

          src = ./.;
        };
        packages.default = pkgs.symlinkJoin {
          name = "stack-lang-1.1.0";
          paths = [config.packages.slc config.packages.stackls];
        };
        packages.checker = pkgs.writeShellApplication {
          name = "checker";
          text = builtins.readFile ./scripts/checker.sh;
        };
        checks.lsp = pkgs.runCommand "stackls-protocol" {nativeBuildInputs = [pkgs.python3];} ''
          python3 ${./tests/lsp/protocol.py} ${config.packages.default}/bin/stackls
          touch $out
        '';
        devShells.default =
          pkgs.mkShell
          {
            name = "env-shell";

            nativeBuildInputs = with pkgs; [
              fasm
              valgrind
              python3
            ];
          };
      };
    };
}
