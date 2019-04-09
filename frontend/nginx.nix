{ nginxRoot }:
let pkgs = import <nixpkgs> {};
    nginxConf = pkgs.writeText "nginx.conf" ''
    worker_processes 1;
    daemon off;
    error_log stderr;
    pid /tmp/nginx.pid;

    events {
        worker_connections 128;
    }

    http {
      access_log /dev/stdout;
      log_format  main  '$remote_addr - $remote_user [$time_local] "$request" '
                        '$status $body_bytes_sent "$http_referer" '
                        '"$http_user_agent" "$http_x_forwarded_for"';

      include             ${pkgs.nginx}/conf/mime.types;
      sendfile            on;
      tcp_nopush          on;
      tcp_nodelay         on;
      keepalive_timeout   65;
      types_hash_max_size 2048;

      default_type        application/octet-stream;
      server {
        listen 0.0.0.0:8888;
        server_name default;
        root ${nginxRoot}/static;
        index index.html;
        ssl off;
        location /api {
          proxy_pass http://hspolls_server;
        }
        location / {
          try_files $uri $uri/ /index.html;
          client_max_body_size 5m;
        }
      }
      upstream hspolls_server {
        server localhost:8000;
      }
    }
    '';
    runNginx = pkgs.writeScriptBin "nginx-run" ''
    #! /bin/sh
    mkdir -p /tmp/logs
    ${pkgs.nginx}/bin/nginx -c ${nginxConf} -p /tmp
    '';
in runNginx
