let
  pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2024-04-29.tar.gz") {}; 

  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages)
      RPostgres;
  };

  system_packages = builtins.attrValues {
    inherit (pkgs)
      glibcLocales
      nix
      igraph           # for the R igraph package (may or may not be needed)
      glpk             # provides libglpk.so
      libxml2
      curl
      openssl
      icu
      fontconfig
      freetype
      harfbuzz
      libpng
      libjpeg
      zlib
      fribidi
      pkg-config
      postgresql
      pandoc
      R;

      # Import -dev packages needed for compiling packages like systemfonts, stringi etc
      freetype_dev = pkgs.freetype.dev;
      fontconfig_dev = pkgs.fontconfig.dev;
      icu_dev = pkgs.icu.dev;


  };
in
pkgs.mkShell {
  LANG = "en_US.UTF-8";
  LC_ALL = "en_US.UTF-8";

  buildInputs = [ rpkgs system_packages ];

shellHook = ''
  export LD_LIBRARY_PATH="${pkgs.libxml2.out}/lib:${pkgs.postgresql.lib}/lib:${pkgs.glpk}/lib:${pkgs.icu.out}/lib:${pkgs.curl.out}/lib:${pkgs.openssl.out}/lib:${pkgs.fontconfig.out}/lib:${pkgs.freetype.out}/lib:${pkgs.harfbuzz.out}/lib:${pkgs.libtiff.out}/lib:${pkgs.libpng.out}/lib:${pkgs.libjpeg.out}/lib:${pkgs.zlib.out}/lib:${pkgs.fribidi.out}/lib:$LD_LIBRARY_PATH"

  export PKG_CONFIG_PATH="${pkgs.icu.dev}/lib/pkgconfig:${pkgs.fontconfig.dev}/lib/pkgconfig:${pkgs.freetype.dev}/lib/pkgconfig:${pkgs.harfbuzz.dev}/lib/pkgconfig:${pkgs.libtiff.dev}/lib/pkgconfig:${pkgs.libpng.dev}/lib/pkgconfig:${pkgs.libjpeg.dev}/lib/pkgconfig:${pkgs.zlib.dev}/lib/pkgconfig:${pkgs.fribidi.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"

  export INCLUDE_PATH="${pkgs.icu.dev}/include:${pkgs.fontconfig.dev}/include:${pkgs.freetype.dev}/include:$INCLUDE_PATH"
  export LIBRARY_PATH="${pkgs.icu.out}/lib:${pkgs.fontconfig.out}/lib:${pkgs.freetype.out}/lib:$LIBRARY_PATH"

  export LANG=en_US.UTF-8
  export LC_ALL=en_US.UTF-8

  echo "🔧 Checking pkg-config availability..."
  pkg-config --cflags fontconfig || echo "fontconfig not found"
  pkg-config --cflags freetype2 || echo "freetype2 not found"
'';

}
