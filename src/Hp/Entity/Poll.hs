module Hp.Entity.Poll
  ( Poll(..)
  , EntityId(..)
  , PollId
  , pollDecoder
  , pollEncoder
  , pollIdDecoder
  , pollIdEncoder
  ) where

import Hp.Entity.User     (UserId, userIdDecoder, userIdEncoder)
import Hp.IsEntity        (IsEntity(..))
import Hp.PollFormElement (PollFormElement)

import Data.Aeson      (eitherDecodeStrict, toJSON)
import Data.Aeson      (FromJSON)
import Data.Time       (DiffTime, UTCTime)
import Data.UUID       (UUID)
import Web.HttpApiData (FromHttpApiData)

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


data Poll
  = Poll
  { created :: UTCTime
  , duration :: DiffTime
  , elements :: Seq PollFormElement
  , userId :: Maybe UserId
  } deriving stock (Generic, Show)

instance IsEntity Poll where
  newtype EntityId Poll
    = PollId { unPollId :: UUID }
    deriving stock (Show)
    deriving newtype (FromHttpApiData, FromJSON)

type PollId
  = EntityId Poll

-- TODO delete this
pollDecoder :: Decoder.Row Poll
pollDecoder =
  Poll
    <$> Decoder.column Decoder.timestamptz
    <*> Decoder.column Decoder.interval
    <*> Decoder.column
          (Decoder.jsonbBytes (over _Left (view packed) . eitherDecodeStrict))
    <*> Decoder.nullableColumn userIdDecoder

-- TODO delete this
pollEncoder :: Encoder.Params (DiffTime, Seq PollFormElement, Maybe UserId)
pollEncoder =
  fold
    [ view _1 >$< Encoder.param Encoder.interval
    , toJSON . view _2 >$< Encoder.param Encoder.jsonb
    , view _3 >$< Encoder.nullableParam userIdEncoder
    ]

pollIdDecoder :: Decoder.Value PollId
pollIdDecoder =
  PollId <$> Decoder.uuid

pollIdEncoder :: Encoder.Params PollId
pollIdEncoder =
  coerce (Encoder.param Encoder.uuid)
