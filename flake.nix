# Follows
# https://nix-community.github.io/home-manager/index.html#sec-flakes-nixos-module
{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
    nur = {
      url = github:nix-community/NUR;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { home-manager, nixpkgs, nur, ... }: {
    nixosConfigurations =
      let
        sharedModules = [
          # Follows
          # https://github.com/nix-community/NUR/tree/287aa9ab138e00c3a3519e437a56b7627c62c478#flake-support
          { nixpkgs.overlays = [ nur.overlay ]; }

          # https://discourse.nixos.org/t/local-flake-based-nix-search-nix-run-and-nix-shell/13433/6
          { nix.nixPath = [ "nixpkgs=${nixpkgs}" ]; }

          # Create a "sys" registry aliasing the nixpkgs flake here to use with `nix search sys`
          # https://discourse.nixos.org/t/local-flake-based-nix-search-nix-run-and-nix-shell/13433/4
          ({ pkgs, ... }: {
            nix.registry.sys = {
              from = { type = "indirect"; id = "sys"; };
              flake = nixpkgs;
            };
            # We could also have nixpkgs pinned to the system version like so:
            # https://gitlab.com/azazel/ender-config/-/blob/6771570f45e0c68b9647f4c4e52dc797d2d6267d/flake.nix#L51-54
            # nix.registry.nixpkgs.flake = nixpkgs;
          })

          ./configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.msk = import ./home.nix;

            # Optionally, use home-manager.extraSpecialArgs to pass
            # arguments to home.nix
          }
        ];
      in
        {
          zen = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              # Hardware
              ./zen-hw-conf.nix
            ] ++ sharedModules;
          };
          xps = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              # Hardware
              ./xps-hw-conf.nix
            ] ++ sharedModules;
          };
        };
  };
}
