{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lispDeps = ps: with ps; [
          fset
          str
          split-sequence
          cl-ppcre
        ];
        sbclWithDeps = pkgs.sbcl.withPackages lispDeps;
      in
        {
        devShell = with pkgs; mkShell {
          buildInputs = [
            sbclWithDeps
          ];
        };
      });
}
