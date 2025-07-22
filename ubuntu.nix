{ pkgs, aikenFlake, currentProfile, project-tracker, image-viewer, cardanoNodeFlake, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "antonio";
  home.homeDirectory = "/home/antonio";
  imports = [
    ( ./sh/zsh.nix)
    ( ./emacs/emacs.nix )
  ];
  
  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  programs.alacritty = {
    package = pkgs.runCommand "alacritty-system" {} ''
      mkdir -p $out/bin
      ln -s /usr/bin/alacritty $out/bin/alacritty
    '';
    enable = true;
    settings = {
      window = {
        padding = {
          x = 8;
          y = 8;
        };
        opacity = 0.9;
      };
      
      font = {
        normal = {
          family = "IBM Plex Mono";
        };
        size = 12.0;
      };
      
      # Esquema de colores similar al que tenías en Kitty
      colors = {
        primary = {
          background = "#1E1E1E";
          foreground = "#DCDCCC";
        };
        
        cursor = {
          text = "#1E1E1E";
          cursor = "#FFCC66";
        };
        
        selection = {
          text = "#DCDCCC";
          background = "#3A3A3A";
        };
        
        normal = {
          black = "#1E1E1E";
          red = "#F2777A";
          green = "#99CC99";
          yellow = "#FFCC66";
          blue = "#6699CC";
          magenta = "#CC99CC";
          cyan = "#66CCCC";
          white = "#DCDCCC";
        };
        
        bright = {
          black = "#7F7F7F";
          red = "#F2777A";
          green = "#99CC99";
          yellow = "#FFCC66";
          blue = "#6699CC";
          magenta = "#CC99CC";
          cyan = "#66CCCC";
          white = "#FFFFFF";
        };
      };
      
      cursor = {
        style = {
          shape = "Beam";
          blinking = "Off";
        };
      };
    };
  };
  # targets.genericLinux.enable = true;
  
  # # Asegurarnos de tener las fuentes necesarias
  # fonts.fontconfig.enable = true;

  # programs.kitty = {
  #   enable = true;
  #   settings = {
  #     # Fondo y transparencia
  #     background = "#1E1E1E";
  #     background_opacity = "0.90";
  #     # Colores del tema Elegant Black
  #     foreground = "#DCDCCC";
  #     cursor = "#FFCC66";
  #     selection_foreground = "#DCDCCC";
  #     selection_background = "#3A3A3A";
  #     url_color = "#66CCCC";
  #     # Definición de colores ANSI
  #     color0  = "#1E1E1E";
  #     color1  = "#F2777A";
  #     color2  = "#99CC99";
  #     color3  = "#FFCC66";
  #     color4  = "#6699CC";
  #     color5  = "#CC99CC";
  #     color6  = "#66CCCC";
  #     color7  = "#DCDCCC";
  #     color8  = "#7F7F7F";
  #     color9  = "#F2777A";
  #     color10 = "#99CC99";
  #     color11 = "#FFCC66";
  #     color12 = "#6699CC";
  #     color13 = "#CC99CC";
  #     color14 = "#66CCCC";
  #     color15 = "#FFFFFF";
  #     # Fuente y tamaño de texto
  #     font_family = "IBM Plex Mono";
  #     font_size = 12;
  #     # Cursor como en Emacs (barra en lugar de bloque)
  #     cursor_shape = "beam";
  #     cursor_blink_interval = 0;
  #     # Color de subrayado como en Emacs
  #     underline_color = "#FFCC66";
  #     # Bordes y márgenes
  #     window_padding_width = 8;
  #     inactive_text_alpha = 0.8;
  #     linux_display_server = "x11";
  #     url_style = "curly";
  #   };
  # };

  nixpkgs.config.allowUnfree = true;

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages =(with pkgs; [
    nix-your-shell
    # gimp does not work correctly
    # gimp
    # obs-studio
    spotify
    ledger-live-desktop
    aikenFlake.packages.${pkgs.system}.aiken
    image-viewer.packages.${pkgs.system}.default
    project-tracker.packages.${pkgs.system}.default
    cardanoNodeFlake.packages.${pkgs.system}.cardano-node
    cardanoNodeFlake.packages.${pkgs.system}.cardano-cli

    nodejs
    nodePackages.typescript
    nodePackages.ts-node
    nodePackages.prettier
    # Language servers
    jdt-language-server
    typescript-language-server
    bash-language-server
    nil

    gnumake
    gcc

    # to compile the mastering cardano book
    bundler
    
  ]);
  programs.home-manager.enable = true;
}
