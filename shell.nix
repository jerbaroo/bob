let
  pkgs = import ./packages.nix;
  project = import ./default.nix;
in
project.shellFor {
  exactDeps = true;
  buildInputs = with pkgs; [
    ghcid
    postgresql
  ];
  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
  };
  withHoogle = false;
}
