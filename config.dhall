{ gitHub =
    ./secrets/github.dhall
    ?
    { clientId = "DummyGitHubClientId"
    , clientSecret = "DummyGitHubClientSecret"
    }

, postgres =
    { host = "127.0.0.1"
    , port = 5432
    , user = "USERNAME"
    , password = "PASSWORD"
    , dbName = "hspolls"
    , poolSize = 20
    , poolTimeout = 5
    }

, port = 8000

, session =
    {
      -- The JSON Web Key used to sign and verify JSON Web Tokens.
      jwk =
        ./secrets/jwk.dhall
        ?
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=="

      -- The session cookie name.
    , name = "Session"

      -- Only send session cookie over TLS.
    , secure = False

      -- How long the session cookie lasts (in seconds). If None, the cookie
      -- will last until the user's browser is closed.
    , ttl = None Natural

      -- Whether or not to use XSRF as implemented by servant-auth.
    , xsrf = False
    }
}
