{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/nixos/nixpkgs/archive/39cb89ef2ffcbef8d2ac2d52267ac69121a6becb.tar.gz"; # nixos-unstable
  sha256 = "1x9vzpay56ap4hgfq1giz00050crdngv5jkxgkxzx276mzhw93na";
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
