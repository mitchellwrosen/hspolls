module Hp.Handler.Subscribe
  ( handleSubscribe
  ) where

import Hp.Eff.PersistUser       (PersistUserEffect)
import Hp.Entity                (Entity)
import Hp.RequestBody.Subscribe (SubscribeRequestBody(..))
import Hp.User                  (User)

import Control.Effect
import Control.Effect.Error (throwError)
import Servant              (NoContent(..), ServerError, err401)
import Servant.Auth.Server  (AuthResult(..))


handleSubscribe ::
     ( Carrier sig m
     , Member (Error ServerError) sig
     , Member PersistUserEffect sig
     )
  => AuthResult (Entity User)
  -> SubscribeRequestBody
  -> m NoContent
handleSubscribe authResult body =
  case authResult of
    Authenticated user ->
      pure NoContent

    _ ->
      throwError err401
