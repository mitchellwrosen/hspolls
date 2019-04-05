-- TODO rename to Poll

module Hp.Form
  ( Form(..)
  ) where

import Hp.PollFormElement

import Data.Aeson (FromJSON)


-- TODO form needs an end time
newtype Form
  = Form (Seq PollFormElement)
  deriving stock (Generic)
  deriving anyclass (FromJSON)
