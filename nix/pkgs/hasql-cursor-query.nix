{ mkDerivation, base, base-prelude, bytestring, contravariant
, foldl, hasql, hasql-cursor-transaction, hasql-transaction
, profunctors, QuickCheck, quickcheck-instances, rebase, stdenv
, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "hasql-cursor-query";
  version = "0.4.4.2";
  sha256 = "09632193fd511749c5ca07f0391f22fdfa6118f1a03658f99a95c2f315e8a7c0";
  libraryHaskellDepends = [
    base base-prelude bytestring contravariant foldl hasql
    hasql-cursor-transaction hasql-transaction profunctors
  ];
  testHaskellDepends = [
    foldl hasql QuickCheck quickcheck-instances rebase tasty
    tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/nikita-volkov/hasql-cursor-query";
  description = "A declarative abstraction over PostgreSQL Cursor";
  license = stdenv.lib.licenses.mit;
}
