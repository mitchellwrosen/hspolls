{-# LANGUAGE AllowAmbiguousTypes #-}

module Hp.Handler.Login.GitHub.GET
  ( handleGetLoginGitHub
  ) where

import Hp.Eff.GitHubAuth  (GitHubAuthEffect, gitHubAuth)
import Hp.Eff.PersistUser (PersistUserEffect, putUserByGitHubUserName)
import Hp.GitHub.Code     (GitHubCode)
import Hp.User            (User)
import Hp.UserId          (UserId(..))

import Control.Effect
import Control.Effect.Reader
import Servant               (Header, Headers, NoContent(..), addHeader,
                              noHeader)
import Servant.Auth.Server   (CookieSettings, JWTSettings, SetCookie,
                              acceptLogin)

handleGetLoginGitHub ::
     ∀ m sig.
     ( Carrier sig m
     , Member GitHubAuthEffect sig
     , Member PersistUserEffect sig
     , Member (Reader CookieSettings) sig
     , Member (Reader JWTSettings) sig
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
        ask

      jwtSettings :: JWTSettings <-
        ask

      user :: User UserId <-
        putUserByGitHubUserName (user ^. #login)

      liftIO (acceptLogin cookieSettings jwtSettings user) >>= \case
        Nothing ->
          authFailure

        Just applyCookies ->
          pure (redirect (applyCookies NoContent))
  where
    redirect =
      addHeader "/"

    authFailure =
      pure (redirect (noHeader (noHeader NoContent)))
