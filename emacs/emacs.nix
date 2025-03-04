{ config, pkgs, ... }:
{   
  # # Instalar Emacs y configuraciones básicas
  programs.home-manager.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29.override {
    # package = pkgs.emacs29.override {
      withTreeSitter = true;
    };
    extraPackages = epkgs: [
      epkgs.magit
	    epkgs.company
	    epkgs.company-quickhelp
      # epkgs.treesit-grammars.with-all-grammars
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

      epkgs.typescript-mode
      epkgs.web-mode
      epkgs.tree-sitter
      # epkgs.tree-sitter-langs
      epkgs.treesit-grammars.with-all-grammars
      epkgs.treesit-auto
      epkgs.rust-mode

      #  Search stack
      epkgs.vertico
      epkgs.orderless
      epkgs.consult
      
    ];
  };
  home.file.".emacs".source = builtins.path {
    path = ./.emacs;
  };
  home.file.".emacs.d/themes/elegant-black-theme.el".source = builtins.path {
    path = ./elegant-black-theme.el;
  };
  home.packages = with pkgs; [
    nerd-fonts.fira-code
  ];
}
