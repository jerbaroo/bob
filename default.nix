let
  pkgs = import ./packages.nix;
in
pkgs.haskell-nix.cabalProject {
  compiler-nix-name = "ghc912";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "agent-bob";
    src = ./.;
  };
}
