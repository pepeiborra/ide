{
  projectFileName ? "cabal.project"
, compiler-nix-name ? "ghc8101"
}:

let
  hsPkgs = import ./. {inherit projectFileName compiler-nix-name};
in
  hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      ghcide
      haskell-language-server
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    tools = { # cabal = "3.2.0.0";
              # hlint = "3.1.6";
            };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    # buildInputs = with pkgs.haskellPackages; [ ghcid ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
