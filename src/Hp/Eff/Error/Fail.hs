module Hp.Eff.Error.Fail
  ( runErrorFail
  ) where

import Hp.Eff.Fail (Fail', fail')

import Control.Effect
import Control.Effect.Error (ErrorC(..), runError)

runErrorFail ::
     ∀ e sig m a.
     ( Carrier sig m
     , Member (Fail' e) sig
     )
  => ErrorC e m a
  -> m a
runErrorFail =
  (either fail' pure =<<) . runError
