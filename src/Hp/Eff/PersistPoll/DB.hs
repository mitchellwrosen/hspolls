{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistPoll.DB
  ( PersistPollDBC(..)
  ) where

import Hp.Eff.DB
import Hp.Eff.PersistPoll (PersistPollEffect(..))
import Hp.Entity          (Entity(..))
import Hp.Poll
import Hp.PollFormElement (PollFormElement)
import Hp.PollId          (PollId(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.Aeson             (toJSON)

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL
import qualified Hasql.Decoders       as D
import qualified Hasql.Encoders       as E
import qualified Hasql.Session        as H
import qualified Hasql.Statement      as H


newtype PersistPollDBC m a
  = PersistPollDBC
  { unPersistPollDBC :: m a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

instance ( Carrier sig m
         , Member DB sig
         ) => Carrier (PersistPollEffect :+: sig) (PersistPollDBC m) where
  eff = PersistPollDBC . \case
    L (SavePoll p k) -> do
      let sql = "INSERT INTO polls (form, end_time) VALUES ($1, $2) RETURNING id"
      runDB (H.statement p (H.Statement sql encodePoll (D.singleRow decodePollId) True)) >>= \case
        Left err -> error (show err)
        -- TODO handle error
        Right pollId -> unPersistPollDBC $ k pollId
    L (GetPoll pollId k) -> do
      let sql = "SELECT form, end_time FROM polls WHERE id = $1"
      runDB (H.statement pollId (H.Statement sql encodePollId ((fmap.fmap) (Entity pollId) (D.rowMaybe decodePoll)) True)) >>= \case
        Left err -> error (show err)
        -- TODO handle error
        Right x -> unPersistPollDBC $ k x
    R other -> eff (handleCoercible other)
    where

encodePoll :: E.Params Poll
encodePoll = mconcat
  [ toJSON . elements >$< E.param E.jsonb
  , endTime >$< E.param E.timestamptz
  ]

decodePoll :: D.Row Poll
decodePoll = Poll
  <$> parseForm
  <*> D.column D.timestamptz
  where
    parseForm :: D.Row (Seq PollFormElement)
    parseForm = D.column
      . D.jsonbBytes
      $ over _Left (view packed) . A.eitherDecode . BL.fromStrict

encodePollId :: E.Params PollId
encodePollId = coerce (E.param E.uuid)

decodePollId :: D.Row PollId
decodePollId = D.column (PollId <$> D.uuid)
