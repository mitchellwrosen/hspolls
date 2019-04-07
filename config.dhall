{
  gitHub =
    ./secrets/github.dhall
    ?
    {
      clientId = "DummyGitHubClientId",
      clientSecret = "DummyGitHubClientSecret"
    },

  port =
    8000,

  session =
    {
      -- The JSON Web Key used to sign and verify JSON Web Tokens.
      jwk =
        ./secrets/jwk.dhall
        ?
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==",

      -- The session cookie name.
      name =
        "hspolls-session",

      -- Only send session cookie over TLS.
      secure =
        False,

      -- How long the session cookie lasts (in seconds). If None, the cookie
      -- will last until the user's browser is closed.
      ttl =
        None Natural
    }
}
