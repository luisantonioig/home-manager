{ config, pkgs, aikenMode, ... }:
{   
  # # Instalar Emacs y configuraciones básicas
  programs.home-manager.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30.override {
      withTreeSitter = true;
    };
    extraPackages = epkgs: [
      epkgs.markdown-mode
      epkgs.magit
	    epkgs.company
	    epkgs.company-quickhelp
	    epkgs.nix-mode
      epkgs.haskell-mode
      epkgs.lsp-haskell
      epkgs.lsp-mode
      epkgs.lsp-ui
	    epkgs.json-mode
      epkgs.consult
      epkgs.beacon
      epkgs.flycheck
      epkgs.projectile
      epkgs.doom-modeline
      epkgs.nerd-icons
      epkgs.dashboard
      epkgs.dash
      epkgs.dash-functional

      epkgs.typescript-mode
      epkgs.web-mode
      # (epkgs.tree-sitter.overrideAttrs (old: {
      #   version = "0.20.0"; # Una versión conocida compatible con ABI 13
      # }))
      epkgs.tree-sitter
      epkgs.tree-sitter-langs

      # epkgs.treesit-grammars.with-all-grammars
      # epkgs.treesit-auto
      
      epkgs.rust-mode

      #  Search stack
      epkgs.vertico
      epkgs.orderless
      epkgs.consult

      aikenMode.packages.${pkgs.system}.default
      epkgs.htmlize


    ];
  };

  

  
  
  home.file.".emacs".source = builtins.path {
    path = ./.emacs;
  };
  home.file.".emacs.d/themes/elegant-black-theme.el".source = builtins.path {
    path = ./elegant-black-theme.el;
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
  home.packages = with pkgs; [ 
    ripgrep
    nerd-fonts.fira-code
    vim-full
  ];
  nixpkgs.overlays = [
  (final: prev: {
    emacs-tree-sitter-grammars = prev.emacs-tree-sitter-grammars.overrideAttrs (old: {
      # Forzar reconstrucción con la versión más reciente de tree-sitter
      buildInputs = old.buildInputs ++ [ final.tree-sitter ];
      # No especificar versión para usar la más reciente
    });
  })
];
}
