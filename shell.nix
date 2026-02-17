{
  pkgs,
  extraPkgs,
  ponysay-fortune-server-core,
  ...
}:

pkgs.mkShell {
  packages = with pkgs.haskellPackages; [
    cabal-install
    ghc
    haskell-language-server
  ];
  inputsFrom = [ ponysay-fortune-server-core ];
}
