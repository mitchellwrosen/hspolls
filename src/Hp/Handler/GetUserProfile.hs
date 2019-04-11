module Hp.Handler.GetUserProfile
  ( handleGetUserProfile
  ) where

import Hp.Entity      (Entity)
import Hp.User        (User(..))
import Hp.UserProfile (UserProfile(..))

import Control.Effect
import Servant.Auth.Server (AuthResult(..))


handleGetUserProfile ::
     ( Carrier sig m
     )
  => AuthResult (Entity User)
  -> m UserProfile
handleGetUserProfile = \case
  Authenticated user ->
    pure UserProfile
      { gitHub = user ^. #value . #gitHub
      }

  _ ->
    undefined -- TODO redirect
