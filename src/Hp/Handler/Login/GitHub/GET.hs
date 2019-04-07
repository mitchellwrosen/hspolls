{-# LANGUAGE AllowAmbiguousTypes #-}

module Hp.Handler.Login.GitHub.GET
  ( handleGetLoginGitHub
  ) where

import Hp.Eff.GitHubAuth (GitHubAuthEffect, gitHubAuth)
import Hp.GitHub.Code    (GitHubCode)
import Hp.UserId         (UserId(..))

import Control.Effect
import Control.Effect.Reader
import Servant               (Header, Headers, NoContent(..), addHeader,
                              noHeader)
import Servant.Auth.Server   (CookieSettings, JWTSettings, SetCookie,
                              acceptLogin)

handleGetLoginGitHub ::
     ∀ env m sig.
     ( Carrier sig m
     , HasType CookieSettings env
     , HasType JWTSettings env
     , Member GitHubAuthEffect sig
     , Member (Reader env) sig
     , MonadIO m -- TODO Unfortunate, get rid of this MonadIO
     )
  => GitHubCode
  -> m (Headers
         '[ Header "Location" Text
          , Header "Set-Cookie" SetCookie
          , Header "Set-Cookie" SetCookie
          ]
       NoContent)
handleGetLoginGitHub code =
  gitHubAuth code >>= \case
    Nothing ->
      authFailure

    Just user -> do
      cookieSettings :: CookieSettings <-
        asks @env (^. typed)

      jwtSettings :: JWTSettings <-
        asks @env (^. typed)

      liftIO (acceptLogin cookieSettings jwtSettings (UserId (user ^. #login))) >>= \case
        Nothing ->
          authFailure

        Just applyCookies ->
          pure (redirect (applyCookies NoContent))
  where
    redirect =
      addHeader "/"

    authFailure =
      pure (redirect (noHeader (noHeader NoContent)))
