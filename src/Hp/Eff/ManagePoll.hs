{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.ManagePoll
  ( ManagePoll
  , getPoll
  , savePoll
  , ManagePollDBC(..)
  ) where

import Data.Aeson (toJSON)
import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Hasql.Session as H
import qualified Hasql.Statement as H
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.Eff.DB
import Hp.Poll
import Hp.PollFormElement (PollFormElement)

data ManagePoll (m :: Type -> Type) (k :: Type) where
  GetPoll :: PollId -> (Maybe Poll -> k) -> ManagePoll m k
  SavePoll :: Poll -> (PollId -> k) -> ManagePoll m k
  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect ManagePoll)

savePoll :: (Carrier sig m, Member ManagePoll sig) => Poll -> m PollId
savePoll poll =
  send $ SavePoll poll pure

getPoll :: (Carrier sig m, Member ManagePoll sig) => PollId -> m (Maybe Poll)
getPoll pollId = send $ GetPoll pollId pure

newtype ManagePollDBC m a
  = ManagePollDBC
  { unManagePollDBC :: m a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

instance ( Carrier sig m
         , Member DB sig
         ) => Carrier (ManagePoll :+: sig) (ManagePollDBC m) where
  eff = ManagePollDBC . \case
    L (SavePoll p k) -> do
      let sql = "INSERT INTO polls (form, end_time) VALUES ($1, $2) RETURNING id"
      runDB (H.statement p (H.Statement sql encodePoll (D.singleRow decodePollId) True)) >>= \case
        Left err -> error (show err)
        -- TODO handle error
        Right pollId -> unManagePollDBC $ k pollId
    L (GetPoll pollId k) -> do
      let sql = "SELECT form, end_time FROM polls WHERE id = $1"
      runDB (H.statement pollId (H.Statement sql encodePollId (D.rowMaybe decodePoll) True)) >>= \case
        Left err -> error (show err)
        -- TODO handle error
        Right x -> unManagePollDBC $ k x
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
