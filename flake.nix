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
        let
          lean = lean4-nix.lib.${system}.fromToolchainFile ./lean-toolchain;
        in
        {
          devShells.default = pkgs.mkShell {
            # `gh` and `git` are invoked by the action's subcommands.
            packages = [
              lean
              pkgs.gh
              pkgs.git
            ];
            # `IO.Process.lakeOutput` resolves lake through the Elan proxy
            # rather than `PATH`, so `lake update` finds nothing in a shell that
            # only has lake on `PATH`. CI installs real Elan; here the toolchain
            # provides the same `bin/lake` layout that `ELAN_HOME` expects.
            #
            # Note this pins every package to this toolchain, where Elan would
            # honour each package's own `lean-toolchain`.
            ELAN_HOME = "${lean}";
          };
        };
    };
}
