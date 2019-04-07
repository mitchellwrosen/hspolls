{ mkDerivation, base-prelude, hasql, hspec, resource-pool, stdenv
, time
}:
mkDerivation {
  pname = "hasql-pool";
  version = "0.5.0.1";
  sha256 = "28c67fd0263d3418b51f3a514abbd1527b5dd690da19bcf90899e14de7b056c7";
  libraryHaskellDepends = [ base-prelude hasql resource-pool time ];
  testHaskellDepends = [ base-prelude hasql hspec ];
  homepage = "https://github.com/nikita-volkov/hasql-pool";
  description = "A pool of connections for Hasql";
  license = stdenv.lib.licenses.mit;
}
