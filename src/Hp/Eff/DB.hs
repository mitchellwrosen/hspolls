{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}

module Hp.Eff.DB
  ( DB(..)
  , DBC
  , runDBC
  , runDB
  ) where

import Control.Effect
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Effect.Carrier
import Hasql.Session

import qualified Hasql.Pool as HPool

data DB (m :: * -> *) k
  = ∀ a. RunDB (Session a) (Either HPool.UsageError a -> k)

deriving instance Functor (DB m)

instance HFunctor DB where
  hmap _ = coerce

instance Effect DB where
  handle st hdl (RunDB sess k) = RunDB sess (hdl . (<$ st) . k)

runDB :: (Member DB sig, Carrier sig m) => Session a -> m (Either HPool.UsageError a)
runDB sess = send (RunDB sess pure)

newtype DBC m a
  = DBC
  { unDBC :: ReaderC HPool.Pool m a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

runDBC :: ∀ m a. HPool.Pool -> DBC m a -> m a
runDBC pool = runReader pool . unDBC

instance ( Carrier sig m
         , MonadIO m
         ) => Carrier (DB :+: sig) (DBC m) where
  eff = DBC . \case
    L (RunDB sess k) -> do
      pool :: HPool.Pool <- ask
      unDBC . k =<< liftIO (HPool.use pool sess)
    R other -> eff (R (handleCoercible other))
