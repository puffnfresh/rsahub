with import <nixpkgs> { };

stdenv.mkDerivation {
  name = "rsahub";
  src = ./.;
  buildInputs = [
    (haskellPackages.ghcWithPackages (drv: [
      drv.RSA
      drv.wreq
    ]))
  ];
  buildPhase = ''
    ghc -Wall -hide-package crypto-random -o rsahub --make Main.hs
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv rsahub $out/bin
  '';
}
