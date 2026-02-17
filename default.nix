{
  pkgs ? import (
    fetchTarball "https://github.com/NixOS/nixpkgs/commit/d74a2335ac9c133d6bbec9fc98d91a77f1604c1f"
  ),
}:

(pkgs.callPackage ./package.nix { }).ponysay-fortune-server
