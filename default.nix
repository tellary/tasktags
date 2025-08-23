{}:
let pkgs = import
  ( fetchTarball {
    # https://github.com/NixOS/nixpkgs/tree/nixos-25.05 as of 'Wed Jul  9 19:31:47 WEST 2025'
    url = "https://github.com/NixOS/nixpkgs/archive/88983d4b665fb491861005137ce2b11a9f89f203.tar.gz";
    sha256 = "sha256:154rnp87ygkfr5jfxz2lamhzqm5blrz55flpy3p6l654vafd72pf";
  }) { };
    ghc = pkgs.haskellPackages.ghcWithHoogle
    (pkgs: with pkgs; [
      haskell-language-server
      
      aeson
      base
      bytestring
      containers
      directory
      email-validate
      filepath
      http-client
      http-types
      ini
      lens
      lens-aeson
      optparse-applicative
      pandoc
      parsec
      split
      text
      time
      unordered-containers
      wreq      
    ]);
in {
  togglSubmit = pkgs.stdenv.mkDerivation {
    name = "togglSubmit";
    buildInputs = [ghc pkgs.git];
    projectDir = ./.;
    unpackPhase = ''
      mkdir src
      cp $projectDir/src/*.hs src/
      mkdir app
      cp $projectDir/app/*.hs app/
      '';
    buildPhase = "ghc -isrc app/togglSubmit.hs";
    installPhase = ''
      mkdir -p $out/bin
      cp app/togglSubmit $out/bin
      '';
  };
}

