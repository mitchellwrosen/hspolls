module Hp.Handler.Root.GET
  ( handleGetRoot
  ) where

import Hp.UserId (UserId)

import Control.Effect
import Servant.Auth.Server (AuthResult(..))
import Text.Blaze.Html5


handleGetRoot ::
     ( Carrier sig m
     )
  => AuthResult UserId
  -> m Html
handleGetRoot _ =
  pure "Hello, world!"