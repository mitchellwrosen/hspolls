module Hp.Eff.HttpSession
  ( HttpSessionEffect(..)
  , beginHttpSession
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Servant                (AddHeader)
import Servant.Auth.Server    (SetCookie, ToJWT)


data HttpSessionEffect (m :: Type -> Type) (k :: Type) where
  BeginHttpSession ::
       ( AddHeader "Set-Cookie" SetCookie response0 response1
       , AddHeader "Set-Cookie" SetCookie response1 response2
       , ToJWT session
       )
    => session
    -> response0
    -> (response2 -> k)
    -> HttpSessionEffect m k

  deriving (Effect, HFunctor)
       via (FirstOrderEffect HttpSessionEffect)

deriving stock instance Functor (HttpSessionEffect m)


beginHttpSession ::
     ( AddHeader "Set-Cookie" SetCookie response0 response1
     , AddHeader "Set-Cookie" SetCookie response1 response2
     , Carrier sig m
     , Member HttpSessionEffect sig
     , ToJWT session
     )
  => session
  -> response0
  -> m response2
beginHttpSession session response =
  send (BeginHttpSession session response pure)
