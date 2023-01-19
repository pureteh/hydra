{ pkgs ? import <nixpkgs> {}
}:
pkgs.mkYarnPackage {
  name = "hydra-explorer-ui";
  src = ./. ;
  doCheck = false;
  DISABLE_ESLINT_PLUGIN = "true";
  buildPhase = ''
    export HOME="$NIX_BUILD_TOP"
    yarn run build
  '';

  installPhase = ''
    mv deps/$pname/build $out
  '';

  distPhase = ''
   true
'';
}
