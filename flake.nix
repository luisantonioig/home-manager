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
    cardanoNodeFlake.url = "github:IntersectMBO/cardano-node";
    # aikenMode = {
    #   url = "github:luisantonioig/aiken-mode/ac165240a4a25314b7a2891840059d99f30f35f8";
    #   inputs.nixpks.follows = "nixpkgs";
    # };

    # nix-your-shell = {
    #   url = "github:MercuryTechnologies/nix-your-shell";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    project-tracker.url = "path:/home/antonio/personal/project-tracker";
    image-viewer.url = "path:/home/antonio/personal/image_viewer";
  };

  outputs = { cardanoNodeFlake, project-tracker, image-viewer, aikenFlake, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      commonArgs = {
        inherit image-viewer aikenFlake project-tracker cardanoNodeFlake;
      };
      mkHome = profile: modulePath:
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ modulePath ];
          extraSpecialArgs = commonArgs // { currentProfile = profile; };
        };
    in {
      homeConfigurations = {
        antonio = mkHome "antonio" ./home.nix;
        haskell96 = mkHome "haskell96" ./haskell96.nix;
        ubuntu = mkHome "ubuntu" ./ubuntu.nix;
        contribute-clippy = mkHome "contribute-clippy" ./contribute-clippy.nix;
        rare-evo-2025 = mkHome "rare-evo-2025" ./rare-evo-2025.nix;
      };
    };
}
