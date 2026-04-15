{ }:

let
  pkgs = import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {};
in
pkgs.mkShell {
  packages = with pkgs; [
    ghc
    pkg-config
    expat
    cairo
    pango
    # for cairo
    libXdmcp
    libxcb
    libsysprof-capture
    # pango
    fribidi
    libthai
    libdatrie
    # glib
    pcre2
    # gio
    util-linux
    libselinux
    libsepol
  ];
}
