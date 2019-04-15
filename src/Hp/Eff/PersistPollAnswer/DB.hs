-- | Real database carrier for the PersistPollAnswer effect.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistPollAnswer.DB
  ( runPersistPollAnswerDB
  ) where

import Hp.Eff.DB                (DB, runDB)
import Hp.Eff.PersistPollAnswer (PersistPollAnswerEffect(..))
import Hp.Entity                (Entity(..))
import Hp.Entity.Poll           (PollId, pollIdEncoder)
import Hp.Entity.PollAnswer     (PollAnswer(..), pollAnswerIdDecoder)
import Hp.Entity.User           (UserId, userIdEncoder)
import Hp.PollQuestionAnswer    (PollQuestionAnswer)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.Aeson             (toJSON)
import Hasql.Statement        (Statement(..))

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder
import qualified Hasql.Session  as Hasql (statement)


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
    L (PutPollAnswer answers pollId userId next) ->
      PersistPollAnswerCarrierDB (doPutPollAnswer answers pollId userId) >>=
        next

    R other ->
      PersistPollAnswerCarrierDB (eff (handleCoercible other))

doPutPollAnswer ::
     ( Carrier sig m
     , Member DB sig
     )
  => [PollQuestionAnswer]
  -> PollId
  -> Maybe UserId
  -> m (Entity PollAnswer)
doPutPollAnswer answers pollId userId =
  runDB (Hasql.statement (pollId, answers, userId) statement)

  where
    statement ::
         Statement
           (PollId, [PollQuestionAnswer], Maybe UserId)
           (Entity PollAnswer)
    statement =
      Statement sql encoder decoder True

      where
        sql :: ByteString
        sql =
          "INSERT INTO poll_responses (pollId, response, userId) VALUES ($1, $2, $3) RETURNING id, created_at"

        encoder :: Encoder.Params (PollId, [PollQuestionAnswer], Maybe UserId)
        encoder =
          fold
            [ view _1 >$< Encoder.param pollIdEncoder
            , toJSON . view _2 >$< Encoder.param Encoder.jsonb
            , view _3 >$< Encoder.nullableParam userIdEncoder
            ]

        decoder :: Decoder.Result (Entity PollAnswer)
        decoder =
          Decoder.singleRow
            (do
              pollAnswerId <- Decoder.column pollAnswerIdDecoder
              created <- Decoder.column Decoder.timestamptz
              pure Entity
                { key = pollAnswerId
                , value =
                    PollAnswer
                      { answers = answers
                      , created = created
                      , pollId = pollId
                      , userId = userId
                      }
                })

runPersistPollAnswerDB :: PersistPollAnswerCarrierDB m a -> m a
runPersistPollAnswerDB =
  unPersistPollAnswerCarrierDB
