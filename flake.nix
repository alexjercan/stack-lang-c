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
        pkgs,
        system,
        ...
      }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "slc";
          version = "1.1.0";

          makeFlags = ["PREFIX=$(out)"];

          nativeBuildInputs = with pkgs; [
            clang
            fasm
          ];

          src = ./.;
        };
        packages.checker = pkgs.writeShellApplication {
          name = "checker";
          text = builtins.readFile ./scripts/checker.sh;
        };
        devShells.default =
          pkgs.mkShell
          {
            name = "env-shell";

            nativeBuildInputs = with pkgs; [
              clang
              fasm
              valgrind
            ];
          };
      };
    };
}
