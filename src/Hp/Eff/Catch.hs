{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.Catch
  ( CatchEffect(..)
  , catch
  , runCatch
  ) where

import Hp.Eff.Throw (ThrowEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum

import qualified Control.Effect.Error as Error


data CatchEffect (e :: Type) (m :: Type -> Type) (k :: Type) where
  Catch ::
       m a
    -> (e -> m a)
    -> (a -> k)
    -> CatchEffect e m k

deriving stock instance Functor (CatchEffect e m)

instance Effect (CatchEffect e) where
  handle ::
       Functor f
    => f ()
    -> (forall x. f (m x) -> n (f x))
    -> CatchEffect e m (m a)
    -> CatchEffect e n (n (f a))
  handle state handler (Catch action h next) =
    Catch
      (handler (action <$ state))
      (handler . (<$ state) . h)
      (handler . fmap next)

instance HFunctor (CatchEffect e) where
  hmap :: (forall x. m x -> n x) -> CatchEffect e m k -> CatchEffect e n k
  hmap f (Catch action handler next) =
    Catch (f action) (f . handler) next

catch ::
     forall e m sig a.
     ( Carrier sig m
     , Member (CatchEffect e) sig
     )
  => m a
  -> (e -> m a)
  -> m a
catch action handler =
  send (Catch action handler pure)

newtype CatchCarrier e m a
  = CatchCarrier { unCatchCarrier :: Error.ErrorC e m a }
  deriving newtype (Applicative, Functor, Monad)

instance
     ( Carrier sig m
     , Effect sig
     )
  => Carrier (CatchEffect e :+: ThrowEffect e :+: sig) (CatchCarrier e m) where

  eff ::
       (CatchEffect e :+: ThrowEffect e :+: sig)
         (CatchCarrier e m)
         (CatchCarrier e m a)
    -> CatchCarrier e m a
  eff = \case
    L (Catch action handler next) ->
      CatchCarrier $
        Error.ErrorC $
          runCatch action >>= \case
            Left err -> runCatch (handler err >>= next)
            Right result -> runCatch (next result)

    R (L (Throw err)) ->
      CatchCarrier (Error.throwError err)

    R (R other) ->
      CatchCarrier (eff (R (handleCoercible other)))

runCatch ::
     CatchCarrier e m a
  -> m (Either e a)
runCatch =
  Error.runError . unCatchCarrier
