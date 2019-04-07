module Hp.Handler.Login.GET
  ( handleGetLogin
  ) where

import Control.Effect
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import qualified Data.ByteString as ByteString


handleGetLogin ::
     ( Carrier sig m
     )
  => m Html
handleGetLogin =
  -- TODO set state get param
  -- TODO set redirect_uri get param
  pure $
    a "Log in with GitHub" !
      href
        (unsafeByteStringValue
          (ByteString.concat
            [ "https://github.com/login/oauth/authorize?"
            , "allow_signup=false&"
            , "client_id=0708940f1632f7a953e8"
            ]))
