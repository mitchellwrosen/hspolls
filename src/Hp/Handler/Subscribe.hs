module Hp.Handler.Subscribe
  ( handleSubscribe
  ) where

import Hp.Entity                (Entity)
import Hp.RequestBody.Subscribe (SubscribeRequestBody(..))
import Hp.User                  (User)

import Control.Effect
import Servant             (NoContent(..))
import Servant.Auth.Server (AuthResult(..))


handleSubscribe ::
     ( Carrier sig m
     )
  => AuthResult (Entity User)
  -> SubscribeRequestBody
  -> m NoContent
handleSubscribe _ _ = pure NoContent
