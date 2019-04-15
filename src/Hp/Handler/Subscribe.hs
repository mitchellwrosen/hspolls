module Hp.Handler.Subscribe
  ( handleSubscribe
  ) where

import Hp.Eff.PersistUser       (PersistUserEffect, setUserSubscription)
import Hp.Eff.Throw             (ThrowEffect, throw)
import Hp.Entity                (Entity)
import Hp.Entity.User           (User)
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
  => AuthResult (Entity User)
  -> SubscribeRequestBody
  -> m NoContent
handleSubscribe authResult body =
  case authResult of
    Authenticated user -> do
      setUserSubscription
        (user ^. #key)
        Subscription
          { pollCreated = body ^. #pollCreated }

      pure NoContent

    _ ->
      throw err401
