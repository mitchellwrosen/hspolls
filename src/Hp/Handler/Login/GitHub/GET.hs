module Hp.Handler.Login.GitHub.GET
  ( handleGetLoginGitHub
  ) where

import Hp.Eff.GitHubAuth (GitHubAuthEffect, gitHubAuth)
import Hp.GitHub.Code    (GitHubCode)

import Control.Effect
import Text.Blaze.Html5 (Html, toHtml)


handleGetLoginGitHub ::
     ∀ m sig.
     ( Carrier sig m
     , Member GitHubAuthEffect sig
     )
  => GitHubCode
  -> m Html
handleGetLoginGitHub code =
  gitHubAuth code >>= \case
    Nothing ->
      pure "Couldn't auth"

    Just user ->
      pure (toHtml (show user))
