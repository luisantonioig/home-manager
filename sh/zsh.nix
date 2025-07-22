{ pkgs, currentProfile, ...}:

let
  my-aliases = {
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
    # Aquí tus configuraciones específicas de zsh
    initContent = ''
      function set_prompt() {
        if [[ "$IN_NIX_SHELL" == "pure" || "$IN_NIX_SHELL" == "impure" ]]; then
          PROMPT="%F{red}╭─[🛠 DEVELOPER-MODE]%f %F{red}%~%f
      %F{red}╰─%F{red}λ%f "
        else
          PROMPT="%F{cyan}╭─%F{magenta}%n%f@%F{blue}${currentProfile}%f %F{yellow}%~%f
      %F{cyan}╰─%F{green}❯%f "
        fi
      }

      set_prompt

      precmd_functions+=(set_prompt)

      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '

      bindkey '^P' history-beginning-search-backward
      bindkey '^N' history-beginning-search-forward
      eval "$(fzf --zsh)"
      eval "$(zoxide init zsh)"
      if command -v nix-your-shell > /dev/null; then
        nix-your-shell zsh | source /dev/stdin
      fi
      if [ -f "$HOME/.variables" ]; then
        source "$HOME/.variables"
      fi
    '';
    shellAliases = my-aliases;
    plugins = [
      # Plugins que quieras usar
    ];
    
    # Otras opciones...
  };
  home.sessionVariables = {
    SHELL = "${pkgs.zsh}/bin/zsh";
  };
  programs.bash.enable = true;
  targets.genericLinux.enable = true;
  home.packages = with pkgs; [
    vivid
    disfetch lolcat cowsay onefetch
    gnugrep gnused
    bat eza bottom fd bc
    direnv nix-direnv
    fzf zoxide ripgrep
    ibm-plex
  ];
  home.file.".profile".text = ''
    export SHELL="${pkgs.zsh}/bin/zsh"
    [ -z "$ZSH_VERSION" ]     && exec "$SHELL" -l
  ''; 
}
