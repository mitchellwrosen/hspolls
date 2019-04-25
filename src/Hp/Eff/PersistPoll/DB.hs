{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistPoll.DB
  ( runPersistPollDB
  ) where

import Hp.Eff.DB
import Hp.Eff.PersistPoll (PersistPollEffect(..))
import Hp.Entity          (Entity(..))
import Hp.Entity.Poll
import Hp.Entity.User     (UserId, userIdDecoder, userIdEncoder)
import Hp.Hasql           (statement)
import Hp.PollFormElement (PollFormElement)

import Control.Effect
import Control.Effect.Interpret
import Data.Aeson               (eitherDecodeStrict, toJSON)
import Data.Time                (DiffTime)

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


runPersistPollDB ::
     ( Carrier sig m
     , Member DB sig
     )
  => InterpretC PersistPollEffect m a
  -> m a
runPersistPollDB =
  runInterpret $ \case
    GetPoll pollId next ->
      doGetPoll pollId >>= next

    SavePoll duration elements userId next ->
      doSavePoll duration elements userId >>= next

doGetPoll ::
     ( Carrier sig m
     , Member DB sig
     )
  => PollId
  -> m (Maybe (Entity Poll))
doGetPoll pollId =
  runDB $
    statement
      "SELECT created_at, duration, form, userId FROM polls WHERE id = $1"
      pollId
      (Encoder.param pollIdEncoder)
      (Decoder.rowMaybe
        (do
          created <- Decoder.column Decoder.timestamptz
          duration <- Decoder.column Decoder.interval
          elements <-
            Decoder.column
              (Decoder.jsonbBytes (over _Left (view packed) . eitherDecodeStrict))
          userId <- Decoder.nullableColumn userIdDecoder
          pure (Entity pollId Poll{..})))

doSavePoll ::
     ( Carrier sig m
     , Member DB sig
     )
  => DiffTime
  -> [PollFormElement]
  -> Maybe UserId
  -> m (Entity Poll)
doSavePoll duration elements userId =
  runDB $
    statement
      "INSERT INTO polls (duration, form, userId) VALUES ($1, $2, $3) RETURNING created_at, id"
      (duration, elements, userId)
      (fold
        [ view _1 >$< Encoder.param Encoder.interval
        , toJSON . view _2 >$< Encoder.param Encoder.jsonb
        , view _3 >$< Encoder.nullableParam userIdEncoder
        ])
      (Decoder.singleRow
        (do
          created <- Decoder.column Decoder.timestamptz
          pollId <- Decoder.column pollIdDecoder
          pure Entity
            { key = pollId
            , value =
                Poll
                  { created = created
                  , duration = duration
                  , elements = elements
                  , userId = userId
                  }
            }))
