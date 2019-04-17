module Hp.Eff.GitHubAuth.Http
  ( runGitHubAuthHttp
  ) where

import Hp.Eff.GitHubAuth                           (GitHubAuthEffect(..))
import Hp.Eff.HttpRequest                          (HttpRequestEffect)
import Hp.Eff.Log                                  (LogEffect, log)
import Hp.Eff.Throw                                (ThrowEffect)
import Hp.GitHub                                   (gitHubGetUser, gitHubPostLoginOauthAccessToken)
import Hp.GitHub.ClientId                          (GitHubClientId)
import Hp.GitHub.ClientSecret                      (GitHubClientSecret)
import Hp.GitHub.PostLoginOauthAccessTokenResponse (GitHubPostLoginOauthAccessTokenResponse)
import Hp.GitHub.Response                          (GitHubResponse(..))

import Control.Effect
import Control.Effect.Interpret

import qualified Servant.Client as Servant (ClientError, Response)


runGitHubAuthHttp ::
     ( Carrier sig m
     , Member HttpRequestEffect sig
     , Member LogEffect sig
     , Member (ThrowEffect Servant.ClientError) sig
     )
  => GitHubClientId
  -> GitHubClientSecret
  -> InterpretC GitHubAuthEffect m a
  -> m a
runGitHubAuthHttp clientId clientSecret =
  runInterpret $ \case
    GitHubAuth code next -> do
      result :: Either Servant.Response (GitHubResponse GitHubPostLoginOauthAccessTokenResponse) <-
        gitHubPostLoginOauthAccessToken
          clientId
          clientSecret
          code
          -- TODO type safe link, and get this from the environment
          (Just "http://localhost:8000/oauth/github")
          -- TODO send random state
          Nothing

      case result of
        Left response -> do
          log (show response ^. packed)
          next Nothing

        Right (GitHubResponseError err) -> do
          log (show err ^. packed)
          next Nothing

        Right (GitHubResponseSuccess response) ->
          gitHubGetUser (response ^. #access_token) >>= \case
            Left response -> do
              log (show response ^. packed)
              next Nothing

            Right user ->
              next (Just user)
