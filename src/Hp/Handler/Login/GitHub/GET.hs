module Hp.Handler.Login.GitHub.GET
  ( handleGetLoginGitHub
  ) where

import Hp.Eff.GitHubAuth  (GitHubAuthEffect, gitHubAuth)
import Hp.Eff.HttpSession (HttpSessionEffect, beginHttpSession)
import Hp.Eff.PersistUser (PersistUserEffect, putUserByGitHubUserName)
import Hp.GitHub.Code     (GitHubCode)
import Hp.User            (User)
import Hp.UserId          (UserId(..))

import Control.Effect
import Servant             (Header, Headers, NoContent(..), addHeader, noHeader)
import Servant.Auth.Server (SetCookie)

handleGetLoginGitHub ::
     ∀ m sig.
     ( Carrier sig m
     , Member GitHubAuthEffect sig
     , Member HttpSessionEffect sig
     , Member PersistUserEffect sig
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
      pure (redirect (noHeader (noHeader NoContent)))

    Just gitHubUser -> do
      user :: User UserId <-
        putUserByGitHubUserName (gitHubUser ^. #login)

      redirect <$> beginHttpSession user NoContent

  where
    redirect =
      addHeader "/"
