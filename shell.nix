{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/nixos/nixpkgs/archive/1dc37370c489b610f8b91d7fdd40633163ffbafd.tar.gz"; # nixos-unstable
  sha256 = "1qvfxf83rya7shffvmy364p79isxmzcq4dxa0lkm5b3ybicnd8f4";
}, pkgs ? import nixpkgsSrc { } }:

with pkgs;

mkShell {
  nativeBuildInputs = [
    jq
    yq
    git
    haskellPackages.hpack
    (haskellPackages.ghcWithPackages (hp: with hp; [ shh optparse-generic ]))
  ];
}
