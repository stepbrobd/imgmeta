{
  outputs = { self, nixpkgs, parts, systems } @ inputs: parts.lib.mkFlake { inherit inputs; } {
    systems = import systems;

    flake.overlays.default = _: prev: {
      ocamlPackages = prev.ocaml-ng.ocamlPackages.overrideScope (ocamlFinal: _: {
        imgmeta = ocamlFinal.callPackage ./default.nix { };
      });
    };

    perSystem = { lib, pkgs, system, self', ... }: {
      _module.args.pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      };

      devShells.default = pkgs.mkShell {
        inputsFrom = with self'.packages; [ default ];
        packages = with pkgs; [
          deno
          nixpkgs-fmt
        ] ++ (with ocamlPackages; [
          bisect_ppx
          ppxlib
          ocaml-lsp
          ocamlformat
          utop
        ]);
      };

      formatter = pkgs.writeShellScriptBin "formatter" ''
        pushd "$(${lib.getExe pkgs.git} rev-parse --show-toplevel)" > /dev/null
        set -eoux pipefail
        shopt -s globstar
        ${lib.getExe pkgs.deno} fmt **/*.md **/*.yaml
        ${lib.getExe pkgs.nixpkgs-fmt} .
        ${lib.getExe pkgs.ocamlPackages.dune_3} fmt
        popd
      '';

      packages.default = pkgs.ocamlPackages.imgmeta;
    };
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    parts.url = "github:hercules-ci/flake-parts";
    parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    systems.url = "github:nix-systems/triplet";
  };
}
