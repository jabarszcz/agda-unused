{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:

  let

    packages = nixpkgs.legacyPackages.x86_64-linux;

    # GHC versions to test against.
    # Adjust when nixpkgs adds or drops GHC versions.
    ghcVersions = {
      ghc94  = packages.haskell.packages.ghc94;
      ghc96  = packages.haskell.packages.ghc96;
      ghc98  = packages.haskell.packages.ghc98;
      ghc910 = packages.haskellPackages;
    };

    mkDrv = hpkgs: hpkgs.callCabal2nix "agda-unused" self {};

    mkShell = hpkgs: (mkDrv hpkgs).env.overrideAttrs (oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++ [
        packages.cabal-install
        hpkgs.Agda
      ];
    });

    derivation = mkDrv packages.haskellPackages;

  in

  {
    defaultPackage.x86_64-linux = derivation;

    devShells.x86_64-linux =
      { default = mkShell packages.haskellPackages; }
      // builtins.mapAttrs (_: mkShell) ghcVersions;

    # nix flake check builds only the default GHC.
    checks.x86_64-linux.default = derivation;

    # Full matrix: nix build .#matrix.x86_64-linux.ghc96
    matrix.x86_64-linux = builtins.mapAttrs (_: mkDrv) ghcVersions;

    # GHC+deps bundles for consumption by other flakes.
    ghcWithDeps.x86_64-linux = builtins.mapAttrs (_: mkShell) ghcVersions;
  };
}
