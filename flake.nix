{
  description = "Fourier-tempered Hamiltonian Monte Carlo on the GPU.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    janus.url = "git+ssh://git@github.com:/ajknapp/janus.git";
    janus.inputs.flake-utils.follows = "flake-utils";
    janus.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, janus, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "brazen";

        janus-pkg = janus.packages.${system}.janus;

        derivation =
          { mkDerivation
          , stdenv
          , lib
          , gcc
          , boost
          , base
          , ad
          , arrows
          , hkd
          , lens
          , linear
          , STMonadTrans
          , tasty
          , tasty-discover
          , tasty-hedgehog
          , tasty-hunit
          , transformers
          , vector
          }:
          mkDerivation {
            pname = packageName;
            version = "0.1.0.0";
            src = ./.;
            libraryHaskellDepends =
              [
                base
                ad
                arrows
                janus-pkg
                hkd
                lens
                linear
                STMonadTrans
                transformers
                vector
              ];
            librarySystemDepends = [ boost gcc gcc.cc.lib ];
            testHaskellDepends = [ tasty tasty-discover tasty-hedgehog tasty-hunit ];
            description = "Fourier-tempered Hamiltonian Monte Carlo on the GPU.";
            license = "unknown";
            hydraPlatforms = lib.platforms.none;
          };

        pkg = haskellPackages.callPackage derivation {};

      in {
        packages.${packageName} = pkg;

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [ pkg ];
          buildInputs = with haskellPackages; pkg.env.buildInputs ++ [
            cabal-install
            haskell-language-server
            ghcid
          ];
          shellHook = ''
            export LIBRARY_PATH=${pkgs.lib.getLib pkgs.stdenv.cc.libc}/lib
          '';
          withHoogle = true;
        };

      });
}

