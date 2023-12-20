{ pkgs ? import <nixpkgs> {} }:
with pkgs; (haskellPackages.extend (haskell.lib.compose.packageSourceOverrides {
  proj = ./.;
}))
  .shellFor {
    packages = p: [p.proj];
    withHoogle = true;
    buildInputs = [
      hpack
      cabal-install
      haskell-language-server
    ];
  }
