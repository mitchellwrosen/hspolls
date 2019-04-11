module Hp.Poll
  ( Poll(..)
  , pollDecoder
  , pollEncoder
  ) where

import Hp.IsEntity        (IsEntity(..))
import Hp.PollFormElement (PollFormElement)
import Hp.PollId          (PollId)

import Data.Aeson (eitherDecodeStrict, toJSON)
import Data.Time  (DiffTime, UTCTime)

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


data Poll
  = Poll
  { created :: UTCTime
  , duration :: DiffTime
  , elements :: Seq PollFormElement
  } deriving stock (Generic, Show)

instance IsEntity Poll where
  type EntityId Poll
    = PollId

pollDecoder :: Decoder.Row Poll
pollDecoder =
  Poll
    <$> decodeCreated
    <*> decodeDuration
    <*> decodeElements

  where
    decodeCreated :: Decoder.Row UTCTime
    decodeCreated =
      Decoder.column Decoder.timestamptz

    decodeDuration :: Decoder.Row DiffTime
    decodeDuration =
      Decoder.column Decoder.interval

    decodeElements :: Decoder.Row (Seq PollFormElement)
    decodeElements =
      Decoder.column
        (Decoder.jsonbBytes (over _Left (view packed) . eitherDecodeStrict))

pollEncoder :: Encoder.Params (DiffTime, Seq PollFormElement)
pollEncoder =
  fold
    [ view _1 >$< durationEncoder
    , view _2 >$< elementsEncoder
    ]
  where
    durationEncoder :: Encoder.Params DiffTime
    durationEncoder =
      Encoder.param Encoder.interval

    elementsEncoder :: Encoder.Params (Seq PollFormElement)
    elementsEncoder =
      toJSON >$< Encoder.param Encoder.jsonb
