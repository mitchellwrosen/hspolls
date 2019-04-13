{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.GitHubAuth.Http
  ( runGitHubAuthHttp
  ) where

import Hp.Eff.GitHubAuth                           (GitHubAuthEffect(..))
import Hp.Eff.HttpRequest                          (HttpRequestEffect)
import Hp.Eff.Log                                  (LogEffect, log)
import Hp.GitHub                                   (gitHubGetUser, gitHubPostLoginOauthAccessToken)
import Hp.GitHub.ClientId                          (GitHubClientId)
import Hp.GitHub.ClientSecret                      (GitHubClientSecret)
import Hp.GitHub.Code                              (GitHubCode)
import Hp.GitHub.PostLoginOauthAccessTokenResponse (GitHubPostLoginOauthAccessTokenResponse)
import Hp.GitHub.Response                          (GitHubResponse(..))
import Hp.GitHub.User                              (GitHubUser)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum

import qualified Servant.Client as Servant (ClientError, Response)

-- TODO log failures instead of discarding them

newtype GitHubAuthCarrierHttp m a
  = GitHubAuthCarrierHttp
  { unGitHubAuthCarrierHttp ::
      ReaderC (GitHubClientId, GitHubClientSecret) m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , Member (Error Servant.ClientError) sig
     , Member HttpRequestEffect sig
     , Member LogEffect sig
     )
  => Carrier (GitHubAuthEffect :+: sig) (GitHubAuthCarrierHttp m) where

  eff ::
       (GitHubAuthEffect :+: sig) (GitHubAuthCarrierHttp m) (GitHubAuthCarrierHttp m a)
    -> GitHubAuthCarrierHttp m a
  eff = \case
    L (GitHubAuth code next) ->
      GitHubAuthCarrierHttp $ do
        (clientId, clientSecret) <-
          ask

        doGitHubAuth clientId clientSecret code >>=
          unGitHubAuthCarrierHttp . next

    R other ->
      GitHubAuthCarrierHttp (eff (R (handleCoercible other)))

doGitHubAuth ::
     forall m sig.
     ( Carrier sig m
     , Member (Error Servant.ClientError) sig
     , Member HttpRequestEffect sig
     , Member LogEffect sig
     )
  => GitHubClientId
  -> GitHubClientSecret
  -> GitHubCode
  -> m (Maybe GitHubUser)
doGitHubAuth clientId clientSecret code =
  doPost >>= \case
    Left response -> do
      log (show response ^. packed)
      pure Nothing

    Right (GitHubResponseError err) -> do
      log (show err ^. packed)
      pure Nothing

    Right (GitHubResponseSuccess response) ->
      gitHubGetUser (response ^. #access_token) >>= \case
        Left response -> do
          log (show response ^. packed)
          pure Nothing

        Right user ->
          pure (Just user)

  where
    doPost ::
          m (Either Servant.Response (GitHubResponse GitHubPostLoginOauthAccessTokenResponse))
    doPost =
      gitHubPostLoginOauthAccessToken
        clientId
        clientSecret
        code
        -- TODO type safe link, and get this from the environment
        (Just "http://localhost:8000/oauth/github")
        -- TODO send random state
        Nothing


runGitHubAuthHttp ::
     GitHubClientId
  -> GitHubClientSecret
  -> GitHubAuthCarrierHttp m a
  -> m a
runGitHubAuthHttp clientId clientSecret =
  runReader (clientId, clientSecret) . unGitHubAuthCarrierHttp
