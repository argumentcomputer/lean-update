{
  description = "lean-update dev shell";

  inputs = {
    nixpkgs.follows = "lean4-nix/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    lean4-nix.url = "github:argumentcomputer/lean4-nix";
  };

  outputs =
    inputs@{
      flake-parts,
      lean4-nix,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      perSystem =
        { system, pkgs, ... }:
        {
          devShells.default = pkgs.mkShell {
            # `gh` and `git` are invoked by the action's subcommands.
            packages = [
              (lean4-nix.lib.${system}.fromToolchainFile ./lean-toolchain)
              pkgs.gh
              pkgs.git
            ];
          };
        };
    };
}
