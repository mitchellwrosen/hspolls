module Hp.Handler.Root.GET
  ( handleGetRoot
  ) where

import Control.Effect

import Text.Blaze.Html5


handleGetRoot ::
     ( Carrier sig m
     )
  => m Html
handleGetRoot =
  pure "Hello, world!"
