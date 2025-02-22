{
  description = "Ponysay-Server: a ponysay-as-a-webserver implementation.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    ponysay-modern.url = "github:CrystalSplitter/ponysay-modern/master";
  };
  outputs =
    { self, nixpkgs, ... }@flakeInputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      extraPkgs = flakeInputs.ponysay-modern.packages.${system};
    in
    {
      packages.${system}.default = pkgs.callPackage ./package.nix { inherit extraPkgs; };
    };
}
