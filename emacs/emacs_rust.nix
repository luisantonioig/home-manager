{ config, pkgs, ... }:
{
  services.emacs.enable = true;

  programs.home-manager.enable = true;

  programs.emacs = {
    enable = true;

    package = pkgs.emacs30.override {
      withTreeSitter = true;
    };

    extraPackages = epkgs: [
      # Core editing
      epkgs.company
      epkgs.company-quickhelp
      epkgs.flycheck
      epkgs.projectile

      # LSP
      epkgs.lsp-mode
      epkgs.lsp-ui

      # Search / completion
      epkgs.vertico
      epkgs.orderless
      epkgs.consult

      # UI
      epkgs.doom-modeline
      epkgs.beacon
      epkgs.nerd-icons
      epkgs.dashboard
      epkgs.yascroll

      # Tree-sitter
      epkgs.tree-sitter
      epkgs.tree-sitter-langs

      # Rust (Emacs)
      epkgs.rust-mode
      epkgs.toml-mode
      epkgs.cargo

      # Snippets (útil en Rust)
      epkgs.yasnippet
      epkgs.yasnippet-snippets

      # Utils
      epkgs.magit
      epkgs.markdown-mode
      epkgs.reformatter
      epkgs.htmlize
      epkgs.shfmt
    ];
  };

  home.file.".emacs".source = builtins.path {
    path = ./.emacs_rust;
  };

  home.file.".emacs.d/themes/elegant-black-theme.el".source = builtins.path {
    path = ./elegant-black-theme.el;
  };
  home.file.".emacs.d/themes/elegant-black2-theme.el".source = builtins.path {
    path = ./elegant-black2-theme.el;
  };
  home.file.".emacs.d/themes/elegant-light-theme.el".source = builtins.path {
    path = ./elegant-light-theme.el;
  };
  home.file.".emacs.d/themes/elegant-balanced-theme.el".source = builtins.path {
    path = ./elegant-balanced-theme.el;
  };

  home.file.".emacs.d/lisp/compact-mode.el".source = builtins.path {
    path = ./compact-mode.el;
  };


  nixpkgs.overlays = [
    (final: prev: {
      emacs-tree-sitter-grammars =
        prev.emacs-tree-sitter-grammars.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ final.tree-sitter ];
        });
    })
  ];
}
