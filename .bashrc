# ~/.bashrc: executed by bash(1) for non-login shells.
[ -f /etc/bashrc ] && . /etc/bashrc

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Include any existing system bashrc
if [ -f /etc/bash.bashrc ]; then
    . /etc/bash.bashrc
fi

# Include user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Set Zsh as default shell
export SHELL="${pkgs.zsh}/bin/zsh"
[ -z "$ZSH_VERSION" ] && exec "$SHELL" -l


# Definir colores ANSI
CYAN="\[\e[36m\]"
MAGENTA="\[\e[35m\]"
BLUE="\[\e[34m\]"
YELLOW="\[\e[33m\]"
GREEN="\[\e[32m\]"
RED="\[\e[31m\]"
WHITE="\[\e[97m\]"
RESET="\[\e[0m\]"

# Definir el prompt de Bash
PS1="${CYAN}╭─${MAGENTA}\u${RESET}@${BLUE}\h ${YELLOW}\w${RESET}\n${CYAN}╰─${GREEN}❯ ${RESET}"

# Definir el prompt secundario en la derecha (no es nativo en Bash, pero se puede emular)
RPROMPT="${RED}▂${YELLOW}▄${GREEN}▆${CYAN}█${BLUE}▆${MAGENTA}▄${WHITE}▂${RESET}"

# Deshabilitar el prompt si el terminal es "dumb"
if [ "$TERM" = "dumb" ]; then
    PS1='$ '
fi

# bindkey '^P' history-beginning-search-backward
# bindkey '^N' history-beginning-search-forward
eval "$(fzf --bash)"
eval "$(zoxide init bash)"
