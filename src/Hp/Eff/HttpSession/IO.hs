{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.HttpSession.IO
  ( runHttpSessionIO
  ) where

import Hp.Eff.HttpSession (HttpSessionEffect(..))

import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum
import Servant                (AddHeader)
import Servant.Auth.Server    (CookieSettings, JWTSettings, SetCookie, ToJWT,
                               acceptLogin)


newtype HttpSessionCarrierIO m a
  = HttpSessionCarrierIO
  { unHttpSessionCarrierIO :: ReaderC (CookieSettings, JWTSettings) m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , MonadIO m -- Because of servant-auth-server... only need randomness >_<
     )
  => Carrier (HttpSessionEffect :+: sig) (HttpSessionCarrierIO m) where

  eff ::
       (HttpSessionEffect :+: sig) (HttpSessionCarrierIO m) (HttpSessionCarrierIO m a)
    -> HttpSessionCarrierIO m a
  eff = \case
    L (BeginHttpSession session request next) ->
      HttpSessionCarrierIO $ do
        (cookieSettings, jwtSettings) <-
          ask

        doBeginHttpSession cookieSettings jwtSettings session request >>=
          unHttpSessionCarrierIO . next

    R other ->
      HttpSessionCarrierIO (eff (R (handleCoercible other)))

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

runHttpSessionIO ::
     CookieSettings
  -> JWTSettings
  -> HttpSessionCarrierIO m a
  -> m a
runHttpSessionIO cookieSettings jwtSettings =
  runReader (cookieSettings, jwtSettings) . unHttpSessionCarrierIO
