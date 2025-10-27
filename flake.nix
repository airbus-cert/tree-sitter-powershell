# source: https://github.com/nix-community/poetry2nix/blob/master/templates/app/flake.nix
{
  description = "tree-sitter-powershell";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
      in
      {
        packages = {
          default = self.packages.${system}.myapp;
        };

        # Shell for app dependencies.
        #
        #     nix develop
        #
        # Use this shell for developing your app.
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.gnumake
            pkgs.nixpkgs-fmt

            # Rust
            pkgs-unstable.rustc
            pkgs-unstable.cargo
            pkgs-unstable.rust-analyzer
            pkgs-unstable.rustfmt

            # Tree-sitter
            pkgs.nodejs_23
            pkgs-unstable.emscripten
            pkgs-unstable.binaryen
            (pkgs-unstable.tree-sitter.override {
              webUISupport = true;
						})
          ];
        };
      });
}
