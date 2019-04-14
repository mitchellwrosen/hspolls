module Hp.Handler.GetUserProfile
  ( handleGetUserProfile
  ) where

import Hp.Entity      (Entity)
import Hp.Entity.User (User(..))
import Hp.UserProfile (UserProfile(..))

import Control.Effect
import Control.Effect.Error
import Servant.Auth.Server  (AuthResult(..))
import Servant.Server       (ServerError, err401)


handleGetUserProfile ::
     ( Carrier sig m
     , Member (Error ServerError) sig
     )
  => AuthResult (Entity User)
  -> m UserProfile
handleGetUserProfile = \case
  Authenticated user ->
    pure UserProfile
      { gitHub = user ^. #value . #gitHub
      , subscribedToPollCreated = user ^. #value . #subscribedToPollCreated
      }

  _ ->
    throwError err401
