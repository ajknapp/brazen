{
  description = "Microcanonical Hamiltonian Monte Carlo on the GPU.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    janus.url = "git+ssh://git@github.com:/ajknapp/janus.git";
    janus.inputs.flake-utils.follows = "flake-utils";
    janus.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, janus, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { system = "x86_64-linux"; config.allowUnfree = true; };

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
          , cudaPackages
          , linuxPackages
          , base
          , ad
          , arrows
          , blaze-html
          , clay
          , gnuplot_bin
          , hkd
          , lens
          , linear
          , tasty
          , tasty-discover
          , tasty-hedgehog
          , tasty-hunit
          , temporary
          , transformers
          , vector
          , vector-fft
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
                blaze-html
                clay
                (jailbreakUnbreak hkd)
                janus-pkg
                lens
                linear
                temporary
                transformers
                vector
                vector-fft
              ];
            librarySystemDepends = [ boost gcc gcc.cc.lib cudaPackages.cudatoolkit cudaPackages.libnvjitlink gcc gnuplot_bin linuxPackages.nvidia_x11 ];
            testHaskellDepends = [ tasty tasty-discover tasty-hedgehog tasty-hunit ];
            description = "Microcanonical Hamiltonian Monte Carlo on the GPU.";
            license = "unknown";
            hydraPlatforms = lib.platforms.none;
          };

        pkg = (haskellPackages.override {
          overrides = self: super: rec {
            gnuplot_bin = pkgs.gnuplot;
          };
        }).callPackage derivation {};

      in {
        packages.${packageName} = pkg;

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [ pkg ];
          buildInputs = with haskellPackages; pkg.env.buildInputs ++ [
            cabal-install
            pkgs.clang-tools
            haskell-language-server
            ghcid
          ];
          shellHook = ''
            export PATH=${pkgs.gcc}/bin:$PATH
            export CUDA_PATH=${pkgs.cudaPackages.cudatoolkit}
            export LD_LIBRARY_PATH=${pkgs.linuxPackages.nvidia_x11}/lib:${pkgs.cudaPackages.libnvjitlink.lib}/lib
            export EXTRA_LDFLAGS="-L${pkgs.linuxPackages.nvidia_x11}/lib"
            export EXTRA_CCFLAGS="-I/usr/include"
          '';
          withHoogle = true;
        };

      });
}

