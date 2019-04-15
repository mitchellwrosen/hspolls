module Hp.Entity.Poll
  ( Poll(..)
  , EntityId(..)
  , PollId
  , pollIdDecoder
  , pollIdEncoder
  , isPollExpired
  ) where

import Hp.Eff.GetCurrentTime (GetCurrentTimeEffect, getCurrentTime)
import Hp.Entity.User        (UserId)
import Hp.IsEntity           (IsEntity(..))
import Hp.PollFormElement    (PollFormElement)

import Control.Effect
import Data.Aeson      (FromJSON)
import Data.Time       (DiffTime, NominalDiffTime, UTCTime, diffUTCTime)
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

pollIdEncoder :: Encoder.Value PollId
pollIdEncoder =
  coerce Encoder.uuid

isPollExpired ::
     ( Carrier sig m
     , Member GetCurrentTimeEffect sig
     )
  => Poll
  -> m Bool
isPollExpired poll = do
  now :: UTCTime <-
    getCurrentTime

  let
    elapsed :: NominalDiffTime
    elapsed =
      now `diffUTCTime` (poll ^. #created)

  pure (elapsed >= realToFrac (poll ^. #duration))
