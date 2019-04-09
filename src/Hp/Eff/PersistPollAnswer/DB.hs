-- | Real database carrier for the PersistPollAnswer effect.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistPollAnswer.DB
  ( runPersistPollAnswerDB
  ) where

import Hp.Eff.DB                (DB, runDB)
import Hp.Eff.PersistPollAnswer (PersistPollAnswerEffect(..))
import Hp.Poll                  (PollId)
import Hp.PollAnswer            (PollAnswer)
import Hp.PollAnswerId          (PollAnswerId)
import Hp.UserId                (UserId(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Hasql.Session              (Session)
import Prelude                    hiding (id)


newtype PersistPollAnswerCarrierDB m a
  = PersistPollAnswerCarrierDB { unPersistPollAnswerCarrierDB :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , Member DB sig
     )
  => Carrier (PersistPollAnswerEffect :+: sig) (PersistPollAnswerCarrierDB m) where

  eff ::
       (PersistPollAnswerEffect :+: sig) (PersistPollAnswerCarrierDB m) (PersistPollAnswerCarrierDB m a)
    -> PersistPollAnswerCarrierDB m a
  eff = \case
    L (PutPollAnswer pollId pollAnswer userId next) ->
      PersistPollAnswerCarrierDB (doPutPollAnswer pollId pollAnswer userId) >>= next

    R other ->
      PersistPollAnswerCarrierDB (eff (handleCoercible other))

doPutPollAnswer ::
     ( Carrier sig m
     , Member DB sig
     )
  => PollId
  -> PollAnswer
  -> Maybe UserId
  -> m (Maybe PollAnswerId)
doPutPollAnswer pollId pollAnswer userId =
  runDB session >>= \case
    Left err ->
      -- TODO deal with Hasql.Pool.UsageError how?
      error (show err)

    Right pollAnswerId ->
      pure (Just pollAnswerId)

  where
    -- TODO insert poll answer
    session :: Session PollAnswerId
    session =
      undefined

runPersistPollAnswerDB :: PersistPollAnswerCarrierDB m a -> m a
runPersistPollAnswerDB =
  unPersistPollAnswerCarrierDB
