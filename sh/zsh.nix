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
    e = "emacsclient -c -a ''";
    et = "emacsclient -t -a ''";
    ek = "systemctl --user stop emacs.service";
    er = "systemctl --user restart emacs.service";
    es = "systemctl --user status emacs.service --no-pager";
    codex = "npx -y @openai/codex";
  };
in
{
  programs.zsh = {
    enable = true;
    # Aquí tus configuraciones específicas de zsh
    initContent = ''
      # Si el terminal es muy básico, desactiva zle y usa prompt simple
      if [[ $TERM == "dumb" ]]; then
        unsetopt zle
        PS1='$ '
        return
      fi

      autoload -Uz add-zsh-hook

      set_prompt() {
        if [[ "$IN_NIX_SHELL" == "pure" || "$IN_NIX_SHELL" == "impure" ]]; then
          PROMPT="%F{red}╭─[🛠 DEVELOPER-MODE]%f %F{red}%~%f
%F{red}╰─%F{red}λ%f "
        else
          PROMPT="%F{cyan}╭─%F{magenta}%n%f@%F{blue}${currentProfile}%f %F{yellow}%~%f
%F{cyan}╰─%F{green}❯%f "
        fi
      }

      add-zsh-hook precmd set_prompt
      set_prompt

      bindkey '^P' history-beginning-search-backward
      bindkey '^N' history-beginning-search-forward

      command -v fzf >/dev/null 2>&1 && eval "$(fzf --zsh)"
      command -v zoxide >/dev/null 2>&1 && eval "$(zoxide init zsh)"
      if command -v nix-your-shell >/dev/null 2>&1; then
        nix-your-shell zsh | source /dev/stdin
      fi

      [[ -f "$HOME/.variables" ]] && source "$HOME/.variables"
      if [[ -f "$HOME/.config/secrets/openai.env" ]]; then
        set -a
        source "$HOME/.config/secrets/openai.env"
        set +a
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
    COMPACT_HOME = "/home/antonio/compact_binaries";
  };
  home.sessionPath = [
    "$COMPACT_HOME"
  ];
  programs.bash.enable = false;
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
}