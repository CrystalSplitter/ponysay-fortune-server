{
  pkgs,
  ...
}:

let
  ponysay-server-core = pkgs.haskellPackages.mkDerivation {
    pname = "ponysay-server";
    version = "0.1.0.0";
    src = ./.;
    doHoogle = false;
    doHaddock = false;
    enableSharedLibraries = true;
    isExecutable = true;
    executableHaskellDepends = with pkgs.haskellPackages; [
      base
      blaze-builder
      http-types
      time
      utf8-string
      wai
      warp
    ];
    homepage = "crystalwobsite.gay";
    license = "AGPL-3.0-or-later";
  };
in

pkgs.symlinkJoin {
  name = "ponysay-server";
  paths = with pkgs; [
    ponysay-server-core
    ponysay
    fortune
    # aha is an alternative ansi to html converter.
    # much smaller dependency stack, faster, but
    # needs more post processing. Look into it.
    # aha
    python3Packages.ansi2html
  ];
}
