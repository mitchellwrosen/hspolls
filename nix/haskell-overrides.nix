{ runCommand, cabal2nix, lib, haskellLib, fetchFromGitHub }:

self: super: with haskellLib; with builtins; with lib.strings;
let callLocalPkg = name: pth:
    let src' = lib.cleanSourceWith { filter = filt; src = pth; };
        filt = path: type:
        let isHiddenFile = hasPrefix "." (baseNameOf path);
        in !isHiddenFile;
    in self.callCabal2nix name src' {};
in
{
  hspolls = callLocalPkg "hspolls" ../.;
  fused-effects = super.callPackage ./pkgs/fused-effects.nix {};
  servant = super.servant_0_16;
  servant-server = super.servant-server_0_16;
  servant-client-core = super.servant-client-core_0_16;
  servant-client = super.servant-client_0_16;
  servant-blaze = super.servant-blaze_0_9;
}
