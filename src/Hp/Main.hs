module Hp.Main where

import Hp.API
import Hp.Env
import Hp.Form

import Control.Effect
import Control.Effect.Reader

import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant                  as Servant
import qualified Servant.Server.Generic   as Servant


main :: IO ()
main =
  Warp.run 8000 application

application ::
     Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
application =
  Servant.genericServeTWithContext
    (liftIO . runM . runReader Env)
    API
      { postPollR = postPoll
      }
    Servant.EmptyContext

postPoll ::
     ( Carrier sig m
     )
  => Form
  -> m Servant.NoContent
postPoll _form =
  pure Servant.NoContent
