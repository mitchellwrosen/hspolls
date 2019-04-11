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
import Data.Time              (DiffTime)

import qualified Hasql.Decoders  as D
import qualified Hasql.Encoders  as E
import qualified Hasql.Session   as H
import qualified Hasql.Statement as H


newtype PersistPollDBC m a
  = PersistPollDBC
  { unPersistPollDBC :: m a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

instance ( Carrier sig m
         , Member DB sig
         ) => Carrier (PersistPollEffect :+: sig) (PersistPollDBC m) where
  eff = PersistPollDBC . \case
    L (SavePoll duration elements k) -> do
      doSavePoll duration elements >>=
        unPersistPollDBC . k
    L (GetPoll pollId k) -> do
      let sql = "SELECT created_at, duration, form FROM polls WHERE id = $1"
      runDB (H.statement pollId (H.Statement sql encodePollId ((fmap.fmap) (Entity pollId) (D.rowMaybe pollDecoder)) True)) >>= \case
        Left err -> error (show err)
        -- TODO handle error
        Right x -> unPersistPollDBC $ k x
    R other -> eff (handleCoercible other)
    where

doSavePoll ::
     ( Carrier sig m
     , Member DB sig
     )
  => DiffTime
  -> Seq PollFormElement
  -> m (Entity Poll)
doSavePoll duration elements =
  runDB session >>= \case
    -- TODO handle error
    Left err ->
      error (show err)

    Right poll ->
      pure poll

  where
    session :: H.Session (Entity Poll)
    session =
      H.statement
        (duration, elements)
        statement

    statement :: H.Statement (DiffTime, Seq PollFormElement) (Entity Poll)
    statement =
      H.Statement sql pollEncoder decoder True

      where
        sql :: ByteString
        sql =
          "INSERT INTO polls (duration, form) VALUES ($1, $2) RETURNING created_at, id"

        decoder :: D.Result (Entity Poll)
        decoder =
          D.singleRow
            ((\created pollId ->
              Entity
                pollId
                Poll
                  { created = created
                  , duration = duration
                  , elements = elements
                  })
              <$> D.column D.timestamptz
              <*> decodePollId)

encodePollId :: E.Params PollId
encodePollId = coerce (E.param E.uuid)

decodePollId :: D.Row PollId
decodePollId = D.column (PollId <$> D.uuid)
