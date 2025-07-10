{ pkgs, aikenFlake, project-tracker, image-viewer, cardanoNodeFlake, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "antonio";
  home.homeDirectory = "/home/antonio";
  imports = [
    ( ./sh/zsh.nix )
    #(./git/git.nix)
    (./emacs/emacs.nix)
    #(./sh/zsh.nix)
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
    gimp
    obs-studio
    spotify
    ledger-live-desktop
    #  NOTE: Set the udev rules using sudo ./ledger-live/ledger-udev-setup.sh or
    #  NOTE: I still have to put the udev rules in /etc/udev/rules.d/20-ledger.rules
    #  SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0001|0004|0005|0015|4000|4001|4002|4003|4004|4005|4006|4007|4008|4009|400a|400b|400c|400d|400e|400f|4010|4011|4012|4013|4014|4015|4016|4017|4018|4019|401a|401b|401c|401d|401e|401f", TAG+="uaccess", TAG+="udev-acl"
    # And then run the following command
    # sudo udevadm control --reload-rules && sudo udevadm trigger
    
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
  # home.sessionVariables = {
  #   LIBGL_ALWAYS_SOFTWARE = "1";  # Usa renderizado por software
  #   ALACRITTY_GL_BACKEND = "glx";  # Forzar el backend GLX
  # };
  # home.sessionVariables = {
  #   KITTY_ENABLE_WAYLAND = 0;
  #   GLF_IM_MODULE = "ibus";
  #   LIBGL_ALWAYS_SOFTWARE = "1";
  # };
  
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
