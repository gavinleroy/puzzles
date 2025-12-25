{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        libExt = pkgs.stdenv.hostPlatform.extensions.sharedLibrary;
        pkgs = import nixpkgs { inherit system; };
        glpkSource = pkgs.fetchFromGitHub {
          owner = "neil-lindquist";
          repo = "linear-programming-glpk";
          rev = "068c418e15fbdac559ec1a318cb4618a8ede724a"; 
          hash = "sha256-Z7uXUXuzD7NlPxPlqGmlMADV1yESKpSUWEDfXTHU1K8="; 
        };
        patchedSource = pkgs.runCommand "patched-glpk-source" {} ''
          cp -r ${glpkSource} $out
          chmod -R u+w $out
          
          # We replace "libglpk" with the ABSOLUTE path to the library.
          # This acts as a hard link that bypasses the search logic entirely.
          sed -i "s|\"libglpk\"|\"${pkgs.glpk}/lib/libglpk${libExt}\"|g" $out/src/ffi.lisp
        '';
        sbclWithDeps = pkgs.sbcl.withPackages (ps: with ps; [
          fset
          str
          cl-ppcre
          linear-programming
          (linear-programming-glpk.overrideAttrs (old: {
            src = patchedSource;
            buildInputs = (old.buildInputs or []) ++ [ pkgs.glpk ];
          }))
        ]);

      in
        {
          devShell = with pkgs; mkShell {
            buildInputs = [
              sbclWithDeps
              pkgs.glpk
            ];
            # Optional: Ensure it's also found at runtime in your shell if you run scripts manually
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.glpk ];
          };
        });
}
