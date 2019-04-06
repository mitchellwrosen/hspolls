{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hp.Eff.GitHubAuth.Http
  ( runGitHubAuthHttp
  ) where

import Hp.Eff.GitHubAuth                           (GitHubAuthCarrier(..),
                                                    GitHubAuthEffect(..))
import Hp.Eff.HttpClient                           (HttpClient)
import Hp.GitHub                                   (gitHubGetUser, gitHubPostLoginOauthAccessToken)
import Hp.GitHub.ClientId                          (GitHubClientId)
import Hp.GitHub.ClientSecret                      (GitHubClientSecret)
import Hp.GitHub.PostLoginOauthAccessTokenResponse (GitHubPostLoginOauthAccessTokenResponse)
import Hp.GitHub.Response                          (GitHubResponse(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum

instance
     ( Carrier sig m
     , HasType GitHubClientId env
     , HasType GitHubClientSecret env
     , Member HttpClient sig
     , Member (Reader env) sig
     )
  => Carrier (GitHubAuthEffect :+: sig) (GitHubAuthCarrier "http" env m) where

  eff ::
       (GitHubAuthEffect :+: sig) (GitHubAuthCarrier "http" env m) (GitHubAuthCarrier "http" env m a)
    -> GitHubAuthCarrier "http" env m a
  eff = \case
    L (GitHubAuth code next) ->
      GitHubAuthCarrier $ do
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
            runGitHubAuthHttp (next Nothing)

          Right (GitHubResponseError _) ->
            runGitHubAuthHttp (next Nothing)

          Right (GitHubResponseSuccess response) ->
            gitHubGetUser (response ^. field @"access_token") >>= \case
              Left _ ->
                runGitHubAuthHttp (next Nothing)

              Right user ->
                runGitHubAuthHttp (next (Just user))

runGitHubAuthHttp ::
     ∀ env m a.
     GitHubAuthCarrier "http" env m a
  -> m a
runGitHubAuthHttp (GitHubAuthCarrier m) =
  m
