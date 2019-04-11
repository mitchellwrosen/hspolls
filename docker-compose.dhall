{ services =
    { postgres =
        { image = "postgres:11.2"
        , environment =
            [ "POSTGRES_USER=hspolls"
            ]
        , network_mode = "host"
        , volumes =
            [ { type = "bind"
              , source = "./db/schema.sql"
              , target = "/docker-entrypoint-initdb.d/schema.sql"
              }
            ]
        }

    , prometheus =
        { image = "prom/prometheus:v2.8.1"
        , network_mode = "host"
        , volumes =
            [ { type = "bind"
              , source = "./prometheus.yml"
              , target = "/etc/prometheus/prometheus.yml"
              }
            , { type = "volume"
              , source = "prometheus"
              , target = "/prometheus"
              }
            ]
        }
    }

, version = "3.7"

, volumes =
    { postgres = {=}
    , prometheus = {=}
    }
}
