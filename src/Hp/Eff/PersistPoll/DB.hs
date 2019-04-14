{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistPoll.DB
  ( PersistPollDBC(..)
  ) where

import Hp.Eff.DB
import Hp.Eff.PersistPoll (PersistPollEffect(..))
import Hp.Entity          (Entity(..))
import Hp.Poll
import Hp.PollFormElement (PollFormElement)
import Hp.PollId          (pollIdDecoder, pollIdEncoder)
import Hp.UserId          (UserId)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.Time              (DiffTime)

import qualified Hasql.Decoders  as D
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
    L (SavePoll duration elements userId k) -> do
      doSavePoll duration elements userId >>=
        unPersistPollDBC . k

    L (GetPoll pollId k) -> do
      let sql = "SELECT created_at, duration, form FROM polls WHERE id = $1"
      runDB (H.statement pollId (H.Statement sql pollIdEncoder ((fmap.fmap) (Entity pollId) (D.rowMaybe pollDecoder)) True)) >>=
        unPersistPollDBC . k

    R other ->
      eff (handleCoercible other)

doSavePoll ::
     ( Carrier sig m
     , Member DB sig
     )
  => DiffTime
  -> Seq PollFormElement
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
         H.Statement (DiffTime, Seq PollFormElement, Maybe UserId) (Entity Poll)
    statement =
      H.Statement sql pollEncoder decoder True

      where
        sql :: ByteString
        sql =
          "INSERT INTO polls (duration, form, userId) VALUES ($1, $2, $3) RETURNING created_at, id"

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
                  , userId = userId
                  })
              <$> D.column D.timestamptz
              <*> D.column pollIdDecoder)
