module Hp.Env where

import qualified Network.HTTP.Client as Http


data Env
  = Env
  { manager :: Http.Manager
  } deriving stock (Generic)
