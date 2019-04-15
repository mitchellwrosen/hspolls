module Hp.Handler.GetUserProfile
  ( handleGetUserProfile
  ) where

import Hp.Eff.PersistUser (PersistUserEffect, getUserById)
import Hp.Eff.Throw       (ThrowEffect, throw)
import Hp.Entity.User     (UserId)
import Hp.UserProfile     (UserProfile(..))

import Control.Effect
import Servant.Auth.Server (AuthResult(..))
import Servant.Server      (ServerError, err401, err404)


handleGetUserProfile ::
     ( Carrier sig m
     , Member PersistUserEffect sig
     , Member (ThrowEffect ServerError) sig
     )
  => AuthResult UserId
  -> m UserProfile
handleGetUserProfile = \case
  Authenticated userId ->
    getUserById userId >>= \case
      Nothing ->
        throw err404

      Just user ->
        pure UserProfile
          { gitHub = user ^. #value . #gitHub
          , subscribedToPollCreated = user ^. #value . #subscribedToPollCreated
          }

  _ ->
    throw err401
