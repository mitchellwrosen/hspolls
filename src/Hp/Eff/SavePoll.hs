{-# LANGUAGE UndecidableInstances, RankNTypes #-}
module Hp.Eff.SavePoll (SavePoll, savePoll, runSavePollPrint) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.Poll


data SavePoll (m :: Type -> Type) (k :: Type) where
  SavePoll :: Poll -> k -> SavePoll m k
  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect SavePoll)

savePoll :: (Carrier sig m, Member SavePoll sig) => Poll -> m ()
savePoll poll =
  send (SavePoll poll (pure ()))


newtype SavePollPrint m a
  = SavePollPrint (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)


instance (Carrier sig m, MonadIO m) => Carrier (SavePoll :+: sig) (SavePollPrint m) where
  eff ::
       (SavePoll :+: sig) (SavePollPrint m) (SavePollPrint m a)
    -> SavePollPrint m a
  eff =
      interp (liftIO . print)


interp :: (Carrier sig m, MonadIO m) =>
     (forall n. MonadIO n => Poll -> n ())
  -> (SavePoll :+: sig) (SavePollPrint m) (SavePollPrint m a)
  -> SavePollPrint m a
interp action = \case
      L (SavePoll poll next) ->
        SavePollPrint (action poll) >> next

      R other ->
        SavePollPrint (eff (handleCoercible other))


runSavePollPrint :: SavePollPrint m a -> m a
runSavePollPrint (SavePollPrint m) =
  m