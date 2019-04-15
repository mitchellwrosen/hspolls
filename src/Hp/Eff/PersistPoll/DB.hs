{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistPoll.DB
  ( PersistPollCarrierDB
  , runPersistPollDB
  ) where

import Hp.Eff.DB
import Hp.Eff.PersistPoll (PersistPollEffect(..))
import Hp.Entity          (Entity(..))
import Hp.Entity.Poll
import Hp.Entity.User     (UserId, userIdDecoder, userIdEncoder)
import Hp.PollFormElement (PollFormElement)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.Aeson             (eitherDecodeStrict, toJSON)
import Data.Time              (DiffTime)

import qualified Hasql.Decoders  as Decoder
import qualified Hasql.Encoders  as Encoder
import qualified Hasql.Session   as H
import qualified Hasql.Statement as H


newtype PersistPollCarrierDB m a
  = PersistPollCarrierDB
  { unPersistPollCarrierDB :: m a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

instance ( Carrier sig m
         , Member DB sig
         ) => Carrier (PersistPollEffect :+: sig) (PersistPollCarrierDB m) where
  eff = PersistPollCarrierDB . \case
    L (GetPoll pollId k) ->
      doGetPoll pollId >>=
        unPersistPollCarrierDB . k

    L (SavePoll duration elements userId k) ->
      doSavePoll duration elements userId >>=
        unPersistPollCarrierDB . k

    R other ->
      eff (handleCoercible other)

doGetPoll ::
     ( Carrier sig m
     , Member DB sig
     )
  => PollId
  -> m (Maybe (Entity Poll))
doGetPoll pollId =
  runDB (H.statement pollId statement)

  where
    statement =
      H.Statement
        "SELECT created_at, duration, form, userId FROM polls WHERE id = $1"
        (Encoder.param pollIdEncoder)
        (Decoder.rowMaybe decoder)
        True

    decoder :: Decoder.Row (Entity Poll)
    decoder = do
      created <- Decoder.column Decoder.timestamptz
      duration <- Decoder.column Decoder.interval
      elements <-
        Decoder.column
          (Decoder.jsonbBytes (over _Left (view packed) . eitherDecodeStrict))
      userId <- Decoder.nullableColumn userIdDecoder
      pure (Entity pollId Poll{..})


doSavePoll ::
     ( Carrier sig m
     , Member DB sig
     )
  => DiffTime
  -> [PollFormElement]
  -> Maybe UserId
  -> m (Entity Poll)
doSavePoll duration elements userId =
  runDB session

  where
    session :: H.Session (Entity Poll)
    session =
      H.statement
        (duration, elements, userId)
        statement

    statement ::
         H.Statement (DiffTime, [PollFormElement], Maybe UserId) (Entity Poll)
    statement =
      H.Statement sql encoder (Decoder.singleRow decoder) True

      where
        sql :: ByteString
        sql =
          "INSERT INTO polls (duration, form, userId) VALUES ($1, $2, $3) RETURNING created_at, id"

        encoder :: Encoder.Params (DiffTime, [PollFormElement], Maybe UserId)
        encoder =
          fold
            [ view _1 >$< Encoder.param Encoder.interval
            , toJSON . view _2 >$< Encoder.param Encoder.jsonb
            , view _3 >$< Encoder.nullableParam userIdEncoder
            ]

        decoder :: Decoder.Row (Entity Poll)
        decoder = do
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
            }

runPersistPollDB ::
     PersistPollCarrierDB m a
  -> m a
runPersistPollDB =
  unPersistPollCarrierDB
