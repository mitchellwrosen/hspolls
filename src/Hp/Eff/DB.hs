{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.DB
  ( DB(..)
  , DBC
  , runDBC
  , runDB
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.Eff.Throw      (ThrowEffect, throw)
import Hp.Hasql          (Session(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum

import qualified Hasql.Pool    as Hasql

data DB (m :: Type -> Type) (k :: Type) where
  RunDB ::
       Session a
    -> (a -> k)
    -> DB m k

  deriving (Effect, HFunctor) via (FirstOrderEffect DB)

deriving instance Functor (DB m)

runDB ::
     ( Carrier sig m
     , Member DB sig
     )
  => Session a
  -> m a
runDB sess =
  send (RunDB sess pure)

newtype DBC m a
  = DBC
  { unDBC :: ReaderC Hasql.Pool m a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

instance ( Carrier sig m
         , Member (ThrowEffect Hasql.UsageError) sig
         , MonadIO m
         ) => Carrier (DB :+: sig) (DBC m) where
  eff = DBC . \case
    L (RunDB sess k) -> do
      pool :: Hasql.Pool <- ask
      liftIO (Hasql.use pool (unSession sess)) >>= \case
        Left err -> throw err
        Right result -> unDBC (k result)
    R other -> eff (R (handleCoercible other))

runDBC :: forall m a. Hasql.Pool -> DBC m a -> m a
runDBC pool = runReader pool . unDBC
