{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    binrep.url   = "github:raehik/binrep";
    binrep.flake = false;
    rerefined.url   = "github:raehik/rerefined";
    rerefined.flake = false;
    bytezap.url   = "github:raehik/bytezap";
    bytezap.flake = false;
    strongweak.url   = "github:raehik/strongweak";
    strongweak.flake = false;
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
        packages.default  = self'.packages.ghc910-gtvm-hs;
        devShells.default = self'.devShells.ghc910;
        haskellProjects.ghc910 = {
          basePackages = pkgs.haskell.packages.ghc910;
          devShell = defDevShell "ghc910";

          # v https://github.com/phadej/defun/pull/5
          settings.defun-core.jailbreak = true;
          packages.finite-typelits.source = "0.2.1.0";
          packages.vector-sized.source = "1.6.1";

          # 0.1.1.3 is last version before backpack overhaul
          # Nix doesn't support backpack
          packages.lzo.source = "0.1.1.3";
          # 2023-02-20: tests broken b/c a file got left out of sdist
          # https://hub.darcs.net/vmchale/sak/issue/2 (unmerged 2024-09-28)
          settings.lzo.check = false;

          packages.binrep.source = inputs.binrep;

          # waiting for nixpkgs update
          packages.rerefined.source = inputs.rerefined;
          settings.strongweak.broken = false;
          packages.strongweak.source = inputs.strongweak;
          packages.bytezap.source = inputs.bytezap;
        };

        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          devShell = defDevShell "ghc98";

          # 2024-10-01 raehik: broken due to blake3-0.3 not working (idk why)

          # 0.1.1.3 is last version before backpack overhaul
          # Nix doesn't support backpack
          packages.lzo.source = "0.1.1.3";
          # 2023-02-20: tests broken b/c a file got left out of sdist
          # https://hub.darcs.net/vmchale/sak/issue/2 (unmerged 2024-09-28)
          settings.lzo.check = false;

          packages.binrep.source = inputs.binrep;

          # waiting for nixpkgs update
          packages.rerefined.source = inputs.rerefined;
          settings.strongweak.broken = false;
          packages.strongweak.source = inputs.strongweak;
          packages.bytezap.source = inputs.bytezap;
        };

        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          devShell = defDevShell "ghc96";

          # 0.1.1.3 is last version before backpack overhaul
          # Nix doesn't support backpack
          packages.lzo.source = "0.1.1.3";
          # 2023-02-20: tests broken b/c a file got left out of sdist
          # https://hub.darcs.net/vmchale/sak/issue/2 (unmerged 2024-09-28)
          settings.lzo.check = false;

          packages.binrep.source = inputs.binrep;

          # waiting for nixpkgs update
          packages.rerefined.source = inputs.rerefined;
          packages.bytezap.source = inputs.bytezap;
          settings.strongweak.broken = false;
          packages.strongweak.source = inputs.strongweak;
        };

        packages.gtvm-hs-image-ghc910 = pkgs.dockerTools.streamLayeredImage {
          name = "gtvm-hs";
          # `git rev-parse HEAD` on clean, "dev" on dirty
          tag = inputs.self.rev or "dev";
          config.Entrypoint = [ "${pkgs.lib.getExe self'.packages.ghc910-gtvm-hs}" ];
          maxLayers = 120; # less than Docker max layers to allow extending
        };
      };
    };
}
