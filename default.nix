{ compiler ? "ghc864" }:

let pkgs = (import ./nix/world.nix { inherit compiler; }).pkgs;
    ghc = pkgs.haskell.packages."${compiler}";
in
{
  ghc = ghc;
}
