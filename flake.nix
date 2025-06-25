{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  }:
  let 
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {
    devShell."${system}" = pkgs.mkShell rec {
      packages = with pkgs; [
        rlwrap
        sbcl
        lispPackages.quicklisp
        ncurses
        libtcod
        # sbclPackages.tcod
        sbclPackages.cffi-libffi
        libffi
      ];
      LD_LIBRARY_PATH = "${nixpkgs.lib.makeLibraryPath packages}";
    };
  };
}
