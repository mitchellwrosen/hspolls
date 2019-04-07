{
  gitHub = ./secrets/github.dhall

         ? { clientId = "DummyGitHubClientId"
           , clientSecret = "DummyGitHubClientSecret"
           },

  port = 8000
}
