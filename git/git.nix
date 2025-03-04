{ config, pkgs, userSettings, ... }:

{
  home.packages = [ pkgs.git ];
  programs.git.enable = true;
  programs.git.userName = "luisantonioig";
  programs.git.userEmail = "antonio.ibarra.g46@gmail.com";
  programs.git.extraConfig = {
    init.defaultBranch = "main";
    safe.directory = [ ("/home/antonio/.dotfiles")
                       ("/home/antonio/.dotfiles/.git") ];
  };
}
