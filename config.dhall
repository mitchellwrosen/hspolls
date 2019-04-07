let
  github : { id : Text, secret : Text } =
    ./secrets/github.dhall
in
  {
    gitHubClientId = github.id,
    gitHubClientSecret = github.secret,
    port = 8000
  }
