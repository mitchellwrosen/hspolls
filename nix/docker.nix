with (import ./world.nix {}).pkgs;

let
  docker-images-src = fetchFromGitHub {
    owner = "tstat";
    repo = "docker-images-nix";
    rev = "b64a7061d021617b1ab6b4d24b9999bcdc702c51";
    sha256 = "1qwa2ws9ab867rs769lvfd6whn514vnr7x1nnpm0bvp5dr71wzxk";
  };
  buildImages = import docker-images-src;
  myImages = { pkgs, config, lib, ... }:
    { imports = [ "${docker-images-src}/postgresql" ];
      config = {
        postgresql = {
          dev = {
            enable = true;
            package = pkgs.postgresql_11;
            enableTCPIP = true;
    
            authentication = ''
              local all all           trust
              host  all all 0.0.0.0/0 trust
            '';
    
            extraConfig = ''
              log_statement = 'all'
              log_duration = true
              session_preload_libraries = 'auto_explain'
              auto_explain.log_min_duration = 0
              auto_explain.log_analyze = true
            '';
          };
        };
      };
    };
in buildImages [ myImages ]
