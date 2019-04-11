module Hp.Worker.SendEmail
  ( sendEmailWorker
  ) where

import Hp.Eff.Await     (AwaitEffect, await)
import Hp.Eff.SendEmail (SendEmailEffect, sendEmail)
import Hp.Email         (Email(..))

import Control.Effect


sendEmailWorker ::
     ( Carrier sig m
     , Member (AwaitEffect Email) sig
     , Member SendEmailEffect sig
     )
  => m void
sendEmailWorker =
  forever (await >>= sendEmail)
