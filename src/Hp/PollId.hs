module Hp.PollId
  ( PollId(..)
  , pollIdDecoder
  , pollIdEncoder
  ) where

import Data.Aeson      (FromJSON)
import Data.UUID       (UUID)
import Web.HttpApiData (FromHttpApiData)

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


newtype PollId
  = PollId { unPollId :: UUID }
  deriving stock (Show)
  deriving newtype (FromHttpApiData, FromJSON)

pollIdDecoder :: Decoder.Value PollId
pollIdDecoder =
  PollId <$> Decoder.uuid

pollIdEncoder :: Encoder.Params PollId
pollIdEncoder =
  coerce (Encoder.param Encoder.uuid)
