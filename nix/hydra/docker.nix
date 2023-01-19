# Docker images built from our packages. NOTE: These images don't include any
# metadata, as this is only added by the Github workflow.

{ hydraPackages # as defined in packages.nix
, system ? builtins.currentSystem
, nixpkgs ? <nixpkgs>
}:
let
  pkgs = import nixpkgs { inherit system; };
  hydra-explorer-files = import ../../hydra-explorer/ui/package.nix { inherit pkgs ; };
  inherit (pkgs) lib;
in
{
  hydra-node = pkgs.dockerTools.buildImage {
    name = "hydra-node";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-node-static}/bin/hydra-node" ];
    };
  };

  hydra-tools = pkgs.dockerTools.buildImage {
    name = "hydra-tools";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-tools-static}/bin/hydra-tools" ];
    };
  };

  hydra-tui = pkgs.dockerTools.buildImage {
    name = "hydra-tui";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-tui-static}/bin/hydra-tui" ];
    };
  };

  hydraw = pkgs.dockerTools.buildImage {
    name = "hydraw";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydraw-static}/bin/hydraw" ];
    };
  };

  hydra-explorer = pkgs.dockerTools.buildImage {
    name = "hydra-explorer";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${hydraPackages.hydra-explorer-static}/bin/hydra-explorer" ];
    };
    copyToRoot = [ pkgs.dockerTools.binSh
                   (pkgs.runCommand "ui-files" {} ''
                   mkdir $out
      ln -s ${hydra-explorer-files} $out/data
    '') ];
  };
}
