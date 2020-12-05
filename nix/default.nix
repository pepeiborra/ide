{ sources ? import ./sources.nix }:
let
  ourProjects = pkgs: [ pkgs.ghcide
                        pkgs.hie-compat
                        pkgs.haskell-language-server
                        pkgs.hls-plugin-api
                        pkgs.hls-tactics-plugin
                        pkgs.hls-hlint-plugin
                      ];
  allDepends = p: builtins.map (x: x.pname) (p.getBuildInputs.haskellBuildInputs or []);
  ourDirectDependsFun = pkgs: haskellPkgs: pkgs.lib.lists.unique (builtins.concatMap allDepends (ourProjects haskellPkgs));

  overlay = _self: pkgs:
    let sharedOverrides = {
        overrides = self: super:
          let ourDirectDepends = ourDirectDependsFun pkgs self;
          in {
            mkDerivation = args: super.mkDerivation (args // # pkgs.lib.trace ourDirectDepends
                {
                    # skip running tests for Hackage packages
                    doCheck = args.pname != "ghcide" && args.pname != "haskell-language-server";
                    # relax upper bounds
                    jailbreak = args.pname != "jailbreak-cabal";
                    # Use -haddock for direct dependencies
                    buildFlags = pkgs.lib.optional (pkgs.lib.elem args.pname ourDirectDepends) "--ghc-option=-haddock";
                });
              };
            };
        gitignoreSource = (import sources.gitignore { inherit (pkgs) lib; }).gitignoreSource;
        extended = haskellPackages:
          haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
            haskell-language-server = gitignoreSource ../.;
            ghcide = gitignoreSource ../ghcide;
            hie-compat = gitignoreSource ../ghcide/hie-compat;
            hls-plugin-api = gitignoreSource ../hls-plugin-api;
            hls-tactics-plugin = gitignoreSource ../plugins/tactics;
            hls-hlint-plugin = gitignoreSource ../plugins/hls-hlint-plugin;
          });
        in
        {
        inherit gitignoreSource;
        ourHaskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                # relax upper bounds on ghc 8.10.x versions (and skip running tests)
                ghc8101 = extended (pkgs.haskell.packages.ghc8101.override sharedOverrides);
                ghc8102 = extended (pkgs.haskell.packages.ghc8102.override sharedOverrides);
            };
          };
        };

  in import sources.nixpkgs { overlays = [ overlay ] ; config = {allowBroken = true;};}
