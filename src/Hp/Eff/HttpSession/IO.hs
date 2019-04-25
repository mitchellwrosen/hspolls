{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.HttpSession.IO
  ( runHttpSessionIO
  ) where

import Hp.Eff.HttpSession (HttpSessionEffect(..))

import Control.Effect.Carrier
import Control.Effect.Interpret
import Servant                  (AddHeader)
import Servant.Auth.Server      (CookieSettings, JWTSettings, SetCookie, ToJWT,
                                 acceptLogin)


runHttpSessionIO ::
     ( Carrier sig m
     , MonadIO m -- Because of servant-auth-server... only need randomness >_<
     )
  => CookieSettings
  -> JWTSettings
  -> InterpretC HttpSessionEffect m a
  -> m a
runHttpSessionIO cookieSettings jwtSettings =
  runInterpret $ \case
    BeginHttpSession session request next ->
      doBeginHttpSession cookieSettings jwtSettings session request >>= next

doBeginHttpSession ::
     ( AddHeader "Set-Cookie" SetCookie response0 response1
     , AddHeader "Set-Cookie" SetCookie response1 response2
     , MonadIO m
     , ToJWT session
     )
  => CookieSettings
  -> JWTSettings
  -> session
  -> response0
  -> m response2
doBeginHttpSession cookieSettings jwtSettings session response =
  liftIO (acceptLogin cookieSettings jwtSettings session) >>= \case
    Nothing ->
      -- TODO throw an error if acceptLogin fails (it won't, with valid JWTSettings)
      undefined

    Just applyCookies ->
      pure (applyCookies response)
