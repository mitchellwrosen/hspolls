module Hp.Handler.Subscribe
  ( handleSubscribe
  ) where

import Hp.Eff.PersistUser       (PersistUserEffect, setUserSubscription)
import Hp.Eff.Throw             (ThrowEffect, throw)
import Hp.Entity.User           (UserId)
import Hp.RequestBody.Subscribe (SubscribeRequestBody(..))
import Hp.Subscription          (Subscription(..))

import Control.Effect
import Servant             (NoContent(..), ServerError, err401)
import Servant.Auth.Server (AuthResult(..))


handleSubscribe ::
     ( Carrier sig m
     , Member PersistUserEffect sig
     , Member (ThrowEffect ServerError) sig
     )
  => AuthResult UserId
  -> SubscribeRequestBody
  -> m NoContent
handleSubscribe authResult body =
  case authResult of
    Authenticated userId -> do
      setUserSubscription
        userId
        Subscription
          { pollCreated = body ^. #pollCreated }

      pure NoContent

    _ ->
      throw err401
