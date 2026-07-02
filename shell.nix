let
  pkgs = import ./packages.nix;
  project = import ./default.nix;
in
project.shellFor {
  exactDeps = true;
  buildInputs = [
    pkgs.ghcid
    pkgs.postgresql
  ];
  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
  };
  withHoogle = false;
}
