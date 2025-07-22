{ pkgs, ... }:

let
  haskellPkgs = pkgs.haskell.packages.ghc96;
in {

  home.username = "antonio";
  home.homeDirectory = "/home/antonio";
  imports = [
    
    ( ./sh/zsh.nix      )
    ( ./ubuntu.nix      )
    ( ./emacs/emacs.nix )
  ];

  home.packages = with pkgs; [
    git
    gcc
    gnumake
    haskellPkgs.cabal-install
    haskellPkgs.haskell-language-server
    haskellPkgs.ghc

  ];

}
