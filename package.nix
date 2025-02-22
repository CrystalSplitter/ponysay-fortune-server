{
  pkgs,
  extraPkgs,
  ...
}:

let
  systemDepends = with pkgs; with extraPkgs; [
    ponysay-modern-bash
    fortune
    # aha is an alternative ansi to html converter.
    # much smaller dependency stack, faster, but
    # needs more post processing. Look into it.
    # aha
    python3Packages.ansi2html
  ];

  ponysay-fortune-server-core = pkgs.haskellPackages.mkDerivation {
    pname = "ponysay-fortune-server";
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
      optparse-applicative
      time
      utf8-string
      wai
      warp
    ];
    buildDepends = systemDepends;
    homepage = "crystalwobsite.gay";
    license = "AGPL-3.0-or-later";
  };
in

pkgs.symlinkJoin {
  name = "ponysay-fortune-server";
  paths =
    with pkgs;
    [
      ponysay-fortune-server-core
    ]
    ++ systemDepends;
}
