{
  pkgs,
  extraPkgs,
  ponysay-fortune-server-core,
  ...
}:

pkgs.mkShell {
  packages = with pkgs.haskellPackages; [
    ghc
    cabal-install
    haskell-language-server
  ];
  inputsFrom = [ ponysay-fortune-server-core ];
}
