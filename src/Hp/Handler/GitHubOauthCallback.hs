module Hp.Handler.GitHubOauthCallback
  ( handleGitHubOauthCallback
  ) where

import Hp.Eff.GitHubAuth  (GitHubAuthEffect, gitHubAuth)
import Hp.Eff.HttpSession (HttpSessionEffect, beginHttpSession)
import Hp.Eff.PersistUser (PersistUserEffect, putUserByGitHubUserName)
import Hp.Entity          (Entity)
import Hp.Entity.User     (User)
import Hp.GitHub.Code     (GitHubCode)

import Control.Effect
import Servant             (Header, Headers, NoContent(..), addHeader, noHeader)
import Servant.Auth.Server (SetCookie)

handleGitHubOauthCallback ::
     ( Carrier sig m
     , Member GitHubAuthEffect sig
     , Member HttpSessionEffect sig
     , Member PersistUserEffect sig
     )
  => GitHubCode
  -> m (Headers
         '[ Header "Location" Text
          , Header "Set-Cookie" SetCookie
          , Header "Set-Cookie" SetCookie
          ]
       NoContent)
handleGitHubOauthCallback code =
  gitHubAuth code >>= \case
    Nothing ->
      pure (redirect (noHeader (noHeader NoContent)))

    Just gitHubUser -> do
      user :: Entity User <-
        putUserByGitHubUserName (gitHubUser ^. #login) (gitHubUser ^. #email)

      redirect <$> beginHttpSession (user ^. #key) NoContent

  where
    redirect =
      addHeader "/"
