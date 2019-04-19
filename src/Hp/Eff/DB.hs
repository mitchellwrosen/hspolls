{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.DB
  ( DB(..)
  , DBC
  , runDBC
  , runDB
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.Eff.Throw      (ThrowEffect, throw)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Sum
import Hasql.Transaction      (Transaction)

import qualified Hasql.Pool                 as Hasql
import qualified Hasql.Session              as Hasql (Session)
import qualified Hasql.Transaction          as Hasql (Transaction)
import qualified Hasql.Transaction.Sessions as Hasql (IsolationLevel(..), Mode(..), transaction)

data DB (m :: Type -> Type) (k :: Type) where
  RunDB ::
       Transaction a
    -> (a -> k)
    -> DB m k

  deriving (Effect, HFunctor) via (FirstOrderEffect DB)

deriving instance Functor (DB m)

runDB ::
     ( Carrier sig m
     , Member DB sig
     )
  => Transaction a
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
      liftIO (Hasql.use pool (runTransaction sess)) >>= \case
        Left err -> throw err
        Right result -> unDBC (k result)
    R other -> eff (R (handleCoercible other))

    where
      runTransaction ::
           Hasql.Transaction a
        -> Hasql.Session a
      runTransaction =
        Hasql.transaction Hasql.Serializable Hasql.Write

runDBC :: forall m a. Hasql.Pool -> DBC m a -> m a
runDBC pool = runReader pool . unDBC
