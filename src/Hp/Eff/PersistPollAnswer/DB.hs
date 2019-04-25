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
import Hp.Hasql                 (statement)
import Hp.PollQuestionAnswer    (PollQuestionAnswer)

import Control.Effect
import Control.Effect.Interpret
import Data.Aeson               (eitherDecodeStrict, toJSON)

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


runPersistPollAnswerDB ::
     ( Carrier sig m
     , Member DB sig
     )
  => InterpretC PersistPollAnswerEffect m a
  -> m a
runPersistPollAnswerDB =
  runInterpret $ \case
    GetPollAnswers pollId next ->
      doGetPollAnswers pollId >>= next

    PutPollAnswer answers pollId userId next ->
      doPutPollAnswer answers pollId userId >>= next

doGetPollAnswers ::
     ( Carrier sig m
     , Member DB sig
     )
  => PollId
  -> m (Vector [PollQuestionAnswer])
doGetPollAnswers pollId =
  runDB $
    statement
      "SELECT response FROM poll_responses WHERE pollId = $1"
      pollId
      (Encoder.param pollIdEncoder)
      (Decoder.rowVector
        (Decoder.column
          (Decoder.jsonbBytes (over _Left (view packed) . eitherDecodeStrict))))

doPutPollAnswer ::
     ( Carrier sig m
     , Member DB sig
     )
  => [PollQuestionAnswer]
  -> PollId
  -> Maybe UserId
  -> m (Entity PollAnswer)
doPutPollAnswer answers pollId userId =
  runDB $
    statement
      "INSERT INTO poll_responses (pollId, response, userId) VALUES ($1, $2, $3) RETURNING id, created_at"
      (pollId, answers, userId)
      (fold
        [ view _1 >$< Encoder.param pollIdEncoder
        , toJSON . view _2 >$< Encoder.param Encoder.jsonb
        , view _3 >$< Encoder.nullableParam userIdEncoder
        ])
      (Decoder.singleRow
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
            }))
