{ pkgs, aikenFlake, project-tracker, image-viewer, ... }:
let
  rustNightly = pkgs.rust-bin.nightly.latest.default;

in {
  home.username = "antonio";
  home.homeDirectory = "/home/antonio";

  imports = [
    ( ./sh/zsh.nix )
    ( ./emacs/emacs_rust.nix )
    ( ./modules/terminal/alacritty-system.nix )
  ];

  home.stateVersion = "24.11";

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
