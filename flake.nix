{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    binrep.url   = "github:raehik/binrep";
    binrep.flake = false;
    bytezap.url   = "github:raehik/bytezap";
    bytezap.flake = false;
  };
  outputs = inputs:
  let
    defDevShell = compiler: {
      mkShellArgs.name = "${compiler}";
      hoogle = false;
      tools = _: {
        haskell-language-server = null;
        hlint = null;
        ghcid = null;
      };
    };
  in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc96-gtvm-hs;
        devShells.default = self'.devShells.ghc96;
        haskellProjects.ghc910 = {
          basePackages = pkgs.haskell.packages.ghc910;
          # v https://github.com/phadej/defun/pull/5
          settings.defun-core.jailbreak = true;
          settings.strongweak.broken = false;
          settings.text-icu.check = false; # 2025-09-25: one test fails???
          packages.bytezap.source = inputs.bytezap;
          devShell = defDevShell "ghc98";
        };
        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          devShell = defDevShell "ghc98";

          # 2024-09-28 raehik: broken due to blake3-0.3 not working (idk why)

          settings.strongweak.broken = false;
          settings.text-icu.check = false; # 2025-09-25: one test fails???
          packages.bytezap.source = inputs.bytezap;
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          devShell = defDevShell "ghc96";

          # 0.1.1.3 is last version before backpack overhaul
          # Nix doesn't support backpack
          packages.lzo.source = "0.1.1.3"; # TODO last non-bp
          # 2023-02-20: tests broken b/c a file got left out of sdist
          # https://hub.darcs.net/vmchale/sak/issue/2 (unmerged 2024-09-28)
          settings.lzo.check = false;

          settings.strongweak.broken = false;
          packages.binrep.source = inputs.binrep;
          packages.bytezap.source = inputs.bytezap;
        };
      };
    };
}

          # packages.example.root = ./.;  # This value is detected based on .cabal files
          #overrides = self: super: with pkgs.haskell.lib; {
            # 2023-02-20: tests broken on GHC 9.4
            # https://github.com/well-typed/optics/issues/478
            #optics = dontCheck super.optics;

            #lzo = overrideCabal super.lzo (oa: {
              # 0.1.1.3 is last version before backpack overhaul; Nix doesn't
              # support backpack
              #version = "0.1.1.3";
              #sha256 = "sha256-98R5APzOBIscMi5SgYvjMXO0qEd8jCv2HT9gz/d3iY4=";
              # 2023-02-20: tests broken b/c a file got left out of sdist
              # https://hub.darcs.net/vmchale/sak/issue/2
              #doCheck = false;
              #broken = false;
