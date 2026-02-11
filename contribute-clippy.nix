{ pkgs, aikenFlake, project-tracker, image-viewer, ... }:
let
  rustNightly = pkgs.rust-bin.nightly.latest.default;

in {
  home.username = "antonio";
  home.homeDirectory = "/home/antonio";

  imports = [
    ( ./sh/zsh.nix )
    ( ./emacs/emacs_rust.nix )
  ];

  home.stateVersion = "24.11";

  programs.alacritty = {
    package = pkgs.runCommand "alacritty-system" {} ''
      mkdir -p $out/bin
      ln -s /usr/bin/alacritty $out/bin/alacritty
    '';
    enable = true;
    settings = {
      window = {
        padding = { x = 8; y = 8; };
        opacity = 0.95;
      };

      font = {
        normal = { family = "IBM Plex Mono"; };
        size = 12.0;
      };

      colors = {
        primary = { background = "#0E0E10"; foreground = "#D7D7D9"; };
        cursor = { text = "#0E0E10"; cursor = "#FFD479"; };
        selection = { text = "#D7D7D9"; background = "#1C1C20"; };

        normal = {
          black = "#0E0E10"; red = "#E87A7A"; green = "#9CD6A3"; yellow = "#FFD479";
          blue = "#82A7DD"; magenta = "#C7A1E6"; cyan = "#71D4C3"; white = "#D7D7D9";
        };

        bright = {
          black = "#555555"; red = "#E87A7A"; green = "#9CD6A3"; yellow = "#FFD479";
          blue = "#82A7DD"; magenta = "#C7A1E6"; cyan = "#71D4C3"; white = "#FFFFFF";
        };
      };

      cursor = {
        style = { shape = "Beam"; blinking = "Off"; };
      };
    };
  };

  nixpkgs.config.allowUnfree = true;

  # ---- Rust / Clippy contributor setup ----
  # rustup lives in ~/.rustup and cargo in ~/.cargo, so ensure PATH is correct.
  home.sessionVariables = {
    RUSTUP_HOME = "$HOME/.rustup";
    CARGO_HOME  = "$HOME/.cargo";
  };

  home.sessionPath = [
    "$HOME/.cargo/bin"
  ];

  # Helpful for builds/tests in rust-lang/rust-clippy
  # (openssl / pkg-config are common deps; also Python for scripts).
  home.packages = (with pkgs; [
    typst
    nix-your-shell
    spotify
    ledger-live-desktop
    aikenFlake.packages.${pkgs.system}.aiken
    image-viewer.packages.${pkgs.system}.default
    project-tracker.packages.${pkgs.system}.default

    nodejs
    nodePackages.typescript
    nodePackages.prettier

    # Language servers
    jdt-language-server
    typescript-language-server
    bash-language-server
    nil

    gnumake
    gcc

    # Often needed in Rust projects
    pkg-config
    openssl
    python3

    # Rust toolchain manager (recommended for contributing to rust-lang/*)
    rustNightly

    # Herramientas que NO siempre vienen por defecto
    pkgs.rust-bin.nightly.latest.rustfmt
    pkgs.rust-bin.nightly.latest.clippy

    # Para LSP
    pkgs.rust-analyzer

    # Utils
    pkgs.cargo-expand
    pkgs.ripgrep
  ]);

  programs.home-manager.enable = true;
}
