module Hp.Eff.SendEmail.Noop
  ( runSendEmailNoop
  ) where

import Hp.Eff.SendEmail (SendEmailEffect(..))

import Control.Effect.Interpret

runSendEmailNoop ::
     InterpretC SendEmailEffect m a
  -> m a
runSendEmailNoop =
  runInterpret $ \case
    SendEmail _ next ->
      next
