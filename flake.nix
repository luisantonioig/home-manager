{
  description = "Home Manager configuration of antonio";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    aikenFlake.url = "github:aiken-lang/aiken";
    aikenMode = {
      url = "github:luisantonioig/aiken-mode/ac165240a4a25314b7a2891840059d99f30f35f8";
      inputs.nixpks.follows = "nixpkgs";
    };
    project-tracker.url = "path:/home/antonio/personal/project-tracker";
  };

  outputs = inputs@{ project-tracker, aikenFlake, aikenMode, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations."antonio" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
      };
      homeConfigurations.ubuntu = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          inherit aikenFlake;
          inherit aikenMode;
          inherit project-tracker;
        };
        modules = [ ./ubuntu.nix ];
      };
    };
}
