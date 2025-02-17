{
  description = "Ponysay-Server: a ponysay-as-a-webserver implementation.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };
  outputs =
    { self, nixpkgs, ... }@flakeInputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default = pkgs.callPackage ./default.nix { };
    };
}
