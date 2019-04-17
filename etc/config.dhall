{ aws =
    ./aws.dhall
    ?
    { accessKeyId = "DummyAccessKeyId"
    , secretAccessKey = "DummySecretAccessKey"
    }

, gitHub =
    Some ./github.dhall
    ?
    None { clientId : Text, clientSecret : Text }

, postgres =
    { host = "127.0.0.1"
    , port = 5432
    , user = "hspolls"
    , password = ""
    , dbName = "hspolls"
    , poolSize = 20
    , poolTimeout = 5
    }

, port = 8000

, session =
    { -- The JSON Web Key used to sign and verify JSON Web Tokens. If None,
      -- generates a random JWK at runtime.
      jwk =
        Some ./jwk.dhall
        ?
        None Text

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
