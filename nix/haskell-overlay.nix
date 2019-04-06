{ compiler }:

self: super:
{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${compiler}" = super.haskell.packages."${compiler}".override {
        overrides = super.callPackage ./haskell-overrides.nix
                  { haskellLib = super.haskell.lib;
                  };
      };
    };
  };
}
