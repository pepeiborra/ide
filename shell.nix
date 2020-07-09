{ compiler ? "ghc8101"
# , rev      ? "c4f97342ba8ac84def72328616dd05d005bb4715"
# , sha256   ? "1p2gbisib2jrz4r9b5vzfvmirgmz9sr2ksalngaw908vvg9hsvai"
# , pkgs     ? import <nixpkgs> {}
  , pkgs     ? import ~/scratch/nixpkgs {}
#    import (builtins.fetchTarball {
#      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
#      inherit sha256; }) {
#      config.allowBroken = false;
#      config.allowUnfree = true;
#    }
}:
let gitignoreSrc = import (pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "2ced4519f865341adcb143c5d668f955a2cb997f";
      sha256 = "0fc5bgv9syfcblp23y05kkfnpgh3gssz6vn24frs8dzw39algk2z";
    }) {};

     hask = pkgs.myHaskell;
     haskPackages = hask.packages.${compiler};
     localGhcide = haskPackages.callCabal2nix "ghcide" ../ghcide {};

    haskell-lsp-types-hashable = pkgs.fetchFromGitHub
      { owner = "alanz";
        repo = "haskell-lsp";
        rev = "d48ed58";
        sha256 = "1micda0b4c0p7z73y2q9yfxb2sa2g1q9wvc02acm8sgkdbl34339";
      };
in
haskPackages.developPackage {
  name = "haskell-language-server";
  root = gitignoreSrc.gitignoreSource ./.;

  overrides = self: super: with hask.lib; {
    # Don't run a package's test suite
    # foo = dontCheck super.foo;
    #
    # Don't enforce package's version constraints
    # bar = doJailbreak super.bar;
    #
    # Get a specific hackage version straight from hackage. Unlike the above
    # callHackage approach, this will always succeed if the version is on
    # hackage. The downside is that you have to specify the hash manually.
    # aeson = callHackageDirect {
    #   pkg = "aeson";
    #   ver = "1.4.2.0";
    #   sha256 = "0qcczw3l596knj9s4ha07wjspd9wkva0jv4734sv3z3vdad5piqh";
    # } {};
    #
    # To discover more functions that can be used to modify haskell
    # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
    # <TAB> to get a tab-completed list of functions.
    # lsp-test = dontCheck (super.callHackageDirect {
    #     pkg = "lsp-test";
    #     ver = "0.11.0.1";
    #     sha256 = "085mx5kfxls6y8kyrx0v1xiwrrcazx10ab5j4l5whs4ll44rl1bh";
    # } {});
    cabal-plan = dontHaddockInterfaces super.cabal-plan;
    extra = super.callHackageDirect {
        pkg = "extra";
        ver = "1.7.2";
        sha256 = "1sz6hnnas0ck01zkgcar7nl41nxa6s6vq6aa45534w76gy8dyqpv";
    } {};
    retrie = super.callHackageDirect {
        pkg = "retrie";
        ver = "0.1.1.1";
        sha256 = "0gnp6j35jnk1gcglrymvvn13sawir0610vh0z8ya6599kyddmw7l";
    } {};
    hie-bios = super.callHackageDirect {
        pkg = "hie-bios";
        ver = "0.5.0";
        sha256 = "116nmpva5jmlgc2dgy8cm5wv6cinhzmga1l0432p305074w720r2";
    } {};
    ormolu = super.callHackageDirect {
        pkg = "ormolu";
        ver = "0.1.2.0";
        sha256 = "0ik09adifvj6pvqnak8a60plpmdma4h1l3vf82sz5mbzaf1zw4jx";
    } {};
    ghc-check = super.callHackageDirect {
        pkg = "ghc-check";
        ver = "0.5.0.1";
        sha256 = "1zlbss7h6infzhhpilvkpk50gxypkb2li8fspi69jlll5l7wqi3d";
    } {};
  };
  source-overrides = {
    # Use a specific hackage version using callHackage. Only works if the
    # version you want is in the version of all-cabal-hashes that you have.
    # bytestring = "0.10.8.1";
    #
    # Use a particular commit from github
    # parsec = pkgs.fetchFromGitHub
    #   { owner = "hvr";
    #     repo = "parsec";
    #     rev = "c22d391c046ef075a6c771d05c612505ec2cd0c3";
    #     sha256 = "0phar79fky4yzv4hq28py18i4iw779gp5n327xx76mrj7yj87id3";
    #   };
    # ghc-check = ../../code/ghc-check;
    retrie = ../retrie;
    # shake = ../shake;
    # lsp-test = pkgs.fetchFromGitHub
    #   { owner = "bubba";
    #     repo = "lsp-test";
    #     rev = "f2ff4d582cebcc82e507f33dc5094586de0ee93d";
    #     sha256 = "0ljy6w2lck8zffx72cirsrwpjpd22crj7jvrw8bd0b6l7nkqpkxp";
    #   };
    haskell-lsp-types = haskell-lsp-types-hashable + "/haskell-lsp-types";
  };
  modifier = drv: with hask.lib;
    let drv' = overrideCabal drv (attrs: {
            doCheck = true;
            doBenchmark = true;
            buildTools = (attrs.buildTools or []) ++ [
                haskPackages.ormolu
                localGhcide
            # pkgs.haskell.packages.${compiler}.cabal-install
            #   pkgs.haskell.packages.${compiler}.ghcid
            ];
        });
       drv'' = disableCabalFlag drv "agpl";
    in drv'' // {env = drv'.envFunc {withHoogle = true;};};
}
