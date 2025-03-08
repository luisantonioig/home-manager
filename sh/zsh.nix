{ pkgs, ... }:
let

  # My shell aliases
  myAliases = {
    ls = "eza --icons --group-directories-first --git -l --color=always --no-time --no-user --no-permissions --no-filesize";
    cat = "bat";
    htop = "btm";
    fd = "fd -Lu";
    w3m = "w3m -no-cookie -v";
    neofetch = "disfetch";
    fetch = "disfetch";
    gitfetch = "onefetch";
    cd = "z";
    "," = "comma";
    rb = "$HOME/personal/nixos-config/rebuild.sh";
  };
in
{
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;
    shellAliases = myAliases;
    initExtra = ''      
      PROMPT="%F{cyan}╭─%F{magenta}%n%f@%F{blue}%m%f %F{yellow}%~%f
      %F{cyan}╰─%F{green}❯%f "

      RPROMPT="%F{red}▂%f%F{yellow}▄%f%F{green}▆%f%F{cyan}█%f%F{blue}▆%f%F{magenta}▄%f%F{white}▂%f"

      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '

      bindkey '^P' history-beginning-search-backward
      bindkey '^N' history-beginning-search-forward
      eval "$(fzf --zsh)"
      eval "$(zoxide init zsh)"
    '';  
  };

  programs.bash = {
    enable = true;
    # enableCompletion = true;
    # shellAliases = myAliases;
    # bashrcExtra = ''source ~/.config/programbs.bash'';
  };

  #  TODO @luisantonioig: Does this also work on NixOS??? What it means?
  targets.genericLinux.enable = true;

  home.packages = with pkgs; [
    vivid
    disfetch lolcat cowsay onefetch
    gnugrep gnused
    bat eza bottom fd bc
    direnv nix-direnv
    fzf zoxide ripgrep
  ];

  home.activation = {
    setZshAsDefault = lib.hm.dag.entryAfter ["writeBoundary"] ''
      if ! grep -q "export SHELL=${pkgs.zsh}/bin/zsh" ~/.bashrc; then
        echo "Adding Zsh shell selection to .bashrc"
        echo -e "\n# Added by home-manager\nexport SHELL=${pkgs.zsh}/bin/zsh\n[ -z \"\$ZSH_VERSION\" ] && exec \"\$SHELL\" -l\n" >> ~/.bashrc
      fi
      
      if ! grep -q "export SHELL=${pkgs.zsh}/bin/zsh" ~/.profile; then
        echo "Adding Zsh shell selection to .profile"
        echo -e "\n# Added by home-manager\nexport SHELL=${pkgs.zsh}/bin/zsh\n[ -z \"\$ZSH_VERSION\" ] && exec \"\$SHELL\" -l\n" >> ~/.profile
      fi
    '';
  };
  home.enableNixpgsReleaseCheck = true;
  
  # Asegúrate de que la variable SHELL siempre apunte a Zsh
  home.sessionVariables = {
    SHELL = "${pkgs.zsh}/bin/zsh";
  }; 

  programs.direnv.enable = true;
  programs.direnv.enableZshIntegration = true;
  programs.direnv.nix-direnv.enable = true;
}


# { config, pkgs, ... }:
# {   
#   # # Instalar Emacs y configuraciones básicas
#   programs.zsh = {
#     enable = true;
#     ];
#   };
#   home.file.".zshrc".source = builtins.path {
#     path = ./.zshrc;
#   };
# }

