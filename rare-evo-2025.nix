{ pkgs, cardanoNodeFlake, ... }:
{

  home.username = "antonio";
  home.homeDirectory = "/home/antonio";
  imports = [
    
    ( ./sh/zsh.nix      )
    ( ./ubuntu.nix      )
    ( ./emacs/emacs.nix )
  ];

  home.packages = [
    
    cardanoNodeFlake.packages.${pkgs.system}.cardano-node
    cardanoNodeFlake.packages.${pkgs.system}.cardano-cli
    cardanoNodeFlake.packages.${pkgs.system}.cardano-address
    cardanoNodeFlake.packages.${pkgs.system}.cardano-testnet

  ];
  home.sessionVariables = {
    CARDANO_NODE = "${cardanoNodeFlake.packages.${pkgs.system}.cardano-node}/bin/cardano-node";
    CARDANO_CLI = "${cardanoNodeFlake.packages.${pkgs.system}.cardano-cli}/bin/cardano-cli";
  };

}
