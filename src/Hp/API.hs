module Hp.API where

import Hp.Form

import Servant
import Servant.API.Generic


data API route
  = API
  { postPollR
      :: route
      :- "poll"
      :> ReqBody '[JSON] Form
      :> Post '[JSON] NoContent
  } deriving stock (Generic)
