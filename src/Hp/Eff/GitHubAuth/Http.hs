{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

module Hp.Eff.GitHubAuth.Http
  ( runGitHubAuthHttp
  ) where

import Hp.Eff.GitHubAuth                           (GitHubAuthEffect(..))
import Hp.Eff.HttpClient                           (HttpClient)
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

-- TODO log failures instead of discarding them

newtype GitHubAuthCarrierHttp env m a
  = GitHubAuthCarrierHttp (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , HasType GitHubClientId env
     , HasType GitHubClientSecret env
     , Member HttpClient sig
     , Member (Reader env) sig
     )
  => Carrier (GitHubAuthEffect :+: sig) (GitHubAuthCarrierHttp env m) where

  eff ::
       (GitHubAuthEffect :+: sig) (GitHubAuthCarrierHttp env m) (GitHubAuthCarrierHttp env m a)
    -> GitHubAuthCarrierHttp env m a
  eff = \case
    L (GitHubAuth code next) ->
      GitHubAuthCarrierHttp (doGitHubAuth @env code) >>= next

    R other ->
      GitHubAuthCarrierHttp (eff (handleCoercible other))

doGitHubAuth ::
     ∀ env sig m.
     ( Carrier sig m
     , HasType GitHubClientId env
     , HasType GitHubClientSecret env
     , Member HttpClient sig
     , Member (Reader env) sig
     )
  => GitHubCode
  -> m (Maybe GitHubUser)
doGitHubAuth code = do
  clientId :: GitHubClientId <-
    asks @env (^. typed)

  clientSecret :: GitHubClientSecret <-
    asks @env (^. typed)

  let
    doPost ::
          m (Either SomeException (GitHubResponse GitHubPostLoginOauthAccessTokenResponse))
    doPost =
      gitHubPostLoginOauthAccessToken
        clientId
        clientSecret
        code
        -- TODO type safe link, and get this from the environment
        (Just "http://localhost:8000/login/github")
        -- TODO send random state
        Nothing

  doPost >>= \case
    Left _ ->
      pure Nothing

    Right (GitHubResponseError _) ->
      pure Nothing

    Right (GitHubResponseSuccess response) ->
      gitHubGetUser (response ^. #access_token) >>= \case
        Left _ ->
          pure Nothing

        Right user ->
          pure (Just user)

runGitHubAuthHttp ::
     ∀ env m a.
     GitHubAuthCarrierHttp env m a
  -> m a
runGitHubAuthHttp (GitHubAuthCarrierHttp m) =
  m
