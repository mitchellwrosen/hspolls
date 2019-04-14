module Hp.Entity.Poll
  ( Poll(..)
  , EntityId(..)
  , PollId
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

pollIdDecoder :: Decoder.Value PollId
pollIdDecoder =
  PollId <$> Decoder.uuid

pollIdEncoder :: Encoder.Params PollId
pollIdEncoder =
  coerce (Encoder.param Encoder.uuid)
