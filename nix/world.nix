{ compiler ? "ghc864" }:

let
  haskellOverlay = import ./haskell-overlay.nix { inherit compiler; };
  pkgs = builtins.fetchGit {
    url = "git@github.com:NixOS/nixpkgs-channels.git";
    rev = "d956f2279b8ac02bd9e48cf2a09dcb66383ab6be";
  };


in
{
  src = pkgs;
  pkgs = import pkgs { overlays = [ haskellOverlay ];
                     };
}
