module Hp.Poll
  ( Poll(..)
  , pollDecoder
  , pollEncoder
  ) where

import Hp.IsEntity        (IsEntity(..))
import Hp.PollFormElement (PollFormElement)
import Hp.PollId          (PollId)
import Hp.UserId          (UserId, userIdEncoder, userIdDecoder)

import Data.Aeson (eitherDecodeStrict, toJSON)
import Data.Time  (DiffTime, UTCTime)

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
  type EntityId Poll
    = PollId

pollDecoder :: Decoder.Row Poll
pollDecoder =
  Poll
    <$> Decoder.column Decoder.timestamptz
    <*> Decoder.column Decoder.interval
    <*> Decoder.column
          (Decoder.jsonbBytes (over _Left (view packed) . eitherDecodeStrict))
    <*> Decoder.nullableColumn userIdDecoder

pollEncoder :: Encoder.Params (DiffTime, Seq PollFormElement, Maybe UserId)
pollEncoder =
  fold
    [ view _1 >$< Encoder.param Encoder.interval
    , toJSON . view _2 >$< Encoder.param Encoder.jsonb
    , view _3 >$< Encoder.nullableParam userIdEncoder
    ]
