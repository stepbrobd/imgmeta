{
  outputs = { self, nixpkgs, parts, systems } @ inputs: parts.lib.mkFlake { inherit inputs; } {
    systems = import systems;

    flake.overlays.default = final: prev: {
      ocamlPackages = prev.ocaml-ng.ocamlPackages.overrideScope (ocamlFinal: ocamlPrev: {
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
          nixpkgs-fmt
        ] ++ (with ocamlPackages; [
          ocaml-lsp
          ocamlformat
          utop
        ]);
      };

      formatter = pkgs.writeShellScriptBin "formatter" ''
        set -eoux pipefail
        root="$PWD"
        while [[ ! -f "$root/.git/index" ]]; do
          if [[ "$root" == "/" ]]; then
            exit 1
          fi
          root="$(dirname "$root")"
        done
        pushd "$root" > /dev/null
        ${lib.getExe pkgs.nixpkgs-fmt} .
        ${lib.getExe pkgs.ocamlPackages.dune_3} fmt
        popd
      '';

      packages.default = pkgs.ocamlPackages.imgmeta;
    };
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    parts.url = "github:hercules-ci/flake-parts";
    parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    systems.url = "github:nix-systems/triplet";
  };
}
