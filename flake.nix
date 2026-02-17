{
  description = "Ponysay-Server: a ponysay-as-a-webserver implementation.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ponysay-modern.url = "github:CrystalSplitter/ponysay-modern/master";
  };
  outputs =
    { self, nixpkgs, ... }@flakeInputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      extraPkgs = flakeInputs.ponysay-modern.packages.${system};
    in
    rec {
      devShells.${system} = rec {
        ponysay-fortune-server = (
          pkgs.callPackage ./shell.nix {
            inherit extraPkgs;
            ponysay-fortune-server-core = packages.${system}.ponysay-fortune-server-core;
          }
        );
        default = ponysay-fortune-server;
      };
      packages.${system} = rec {
        ponysay-fortune-server-core =
          (pkgs.callPackage ./package.nix { inherit extraPkgs; }).ponysay-fortune-server-core;
        ponysay-fortune-server =
          (pkgs.callPackage ./package.nix { inherit extraPkgs; }).ponysay-fortune-server;
        default = ponysay-fortune-server;
      };
      formatter.${system} = pkgs.nixfmt-tree;
    };
}
