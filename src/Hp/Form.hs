module Hp.Form
  ( Form(..)
  ) where

import Data.Aeson (FromJSON)


data Form
  = Form
  deriving stock (Generic)
  deriving anyclass (FromJSON)
