{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "antonio";
  home.homeDirectory = "/home/antonio";
  imports = [
    #(./git/git.nix)
    (./emacs/emacs.nix)
    #(./sh/zsh.nix)
    #  TODO @luisantonioig: It seems that google-chrome does not works on ubuntu using home-manager so instead install it using sudo apt get google-chrome
    #(./google-chrome/google-chrome.nix)
    
  ];

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  # programs.kitty = {
  #   enable = true;
    # settings = {
    #   Fondo y transparencia
    #   background = "#1E1E1E";
    #   background_opacity = "0.90";  Ajusta entre 0.0 (transparente) y 1.0 (opaco)
    #   Colores del tema Elegant Black
    #   foreground = "#DCDCCC"; Texto principal
    #   cursor = "#FFCC66";     Cursor amarillo
    #   selection_foreground = "#DCDCCC";
    #   selection_background = "#3A3A3A";
    #   url_color = "#66CCCC";
    #   Definición de colores ANSI
    #   color0  = "#1E1E1E"; Negro
    #   color1  = "#F2777A"; Rojo
    #   color2  = "#99CC99"; Verde
    #   color3  = "#FFCC66"; Amarillo
    #   color4  = "#6699CC"; Azul
    #   color5  = "#CC99CC"; Magenta
    #   color6  = "#66CCCC"; Cian
    #   color7  = "#DCDCCC"; Blanco/gris claro
    #   color8  = "#7F7F7F"; Gris oscuro
    #   color9  = "#F2777A"; Rojo brillante
    #   color10 = "#99CC99"; Verde brillante
    #   color11 = "#FFCC66"; Amarillo brillante
    #   color12 = "#6699CC"; Azul brillante
    #   color13 = "#CC99CC"; Magenta brillante
    #   color14 = "#66CCCC"; Cian brillante
    #   color15 = "#FFFFFF"; Blanco brillante
    #   Fuente y tamaño de texto
    #   font_family = "IBM Plex Mono";
    #   font_size = 12;
    #   Cursor como en Emacs (barra en lugar de bloque)
    #   cursor_shape = "beam";
    #   cursor_blink_interval = 0;
    #   Color de subrayado como en Emacs
    #   underline_color = "#FFCC66";
    #   Bordes y márgenes
    #   window_padding_width = 8;
    #   inactive_text_alpha = 0.8;
    # };
  # };

  # nixpkgs.config.allowUnfree = true;

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages =(with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello
    # google-chrome
    # slack
    
    # ventoy

    # zsh
    # oh-my-zsh
    fzf
    zoxide
    #  TODO @luisantonioig: Looks like this package also does not work for ubuntu, so install it using sudo apt install slack-desktop
    # slack
    ibm-plex
    ripgrep
    
    nodejs_23
    typescript
    typescript-language-server

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ]);
    
    

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  # home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  #   ".config/programbs.bash".source = ./.bashrc;
  # };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/antonio/etc/profile.d/hm-session-vars.sh
  #
  # home.sessionVariables = {
  #   LIBVA_DRIVER_NAME = "nvidia";  # Usar la aceleración por hardware de NVIDIA
  #   __GLX_VENDOR_LIBRARY_NAME = "nvidia";  # Obligar OpenGL a usar NVIDIA
  #   __NV_PRIME_RENDER_OFFLOAD = "1";  # Activar PRIME para usar NVIDIA en procesos gráficos
  #   __NV_PRIME_RENDER_OFFLOAD_PROVIDER = "NVIDIA-G0";  # Seleccionar la GPU dedicada
  #   __GLVND_DISALLOW_PATCHING = "1";  # Evitar errores en la selección de drivers
  # };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
