-- | Real database carrier for the PersistUser effect.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistUser.DB
  ( runPersistUserDB
  ) where

import Hp.Eff.DB          (DB, runDB)
import Hp.Eff.PersistUser
import Hp.Entity          (Entity(..))
import Hp.GitHub.UserName (GitHubUserName(..), gitHubUserNameEncoder)
import Hp.Subscription    (Subscription(..))
import Hp.User            (User(..), userDecoder, userEncoder)
import Hp.UserId          (UserId(..), userIdDecoder, userIdEncoder)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Hasql.Session              (Session, statement)
import Hasql.Statement            (Statement(..))
import Hasql.Transaction.Sessions (IsolationLevel(..), Mode(..), transaction)
import Prelude                    hiding (id)

import qualified Hasql.Decoders    as Decoder
import qualified Hasql.Encoders    as Encoder
import qualified Hasql.Transaction as Transaction (statement)


newtype PersistUserCarrierDB m a
  = PersistUserCarrierDB (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , Member DB sig
     )
  => Carrier (PersistUserEffect :+: sig) (PersistUserCarrierDB m) where

  eff ::
       (PersistUserEffect :+: sig) (PersistUserCarrierDB m) (PersistUserCarrierDB m a)
    -> PersistUserCarrierDB m a
  eff = \case
    L (GetUserEmailsSubscribedToPollCreatedEvents next) ->
      PersistUserCarrierDB doGetUserEmailsSubscribedToPollCreatedEvents >>= next

    L (PutUserByGitHubUserName name email next) ->
      PersistUserCarrierDB (doPutUserByGitHubUserName name email) >>= next

    L (SetUserSubscription userId sub next) -> do
      PersistUserCarrierDB (doSetUserSubscription userId sub)
      next

    R other ->
      PersistUserCarrierDB (eff (handleCoercible other))

doGetUserEmailsSubscribedToPollCreatedEvents ::
     ( Carrier sig m
     , Member DB sig
     )
  => m [Text]
doGetUserEmailsSubscribedToPollCreatedEvents =
  runDB session >>= \case
    Left err -> do
      traceShowM err
      undefined

    Right emailAddresses ->
      pure emailAddresses

  where
    session :: Session [Text]
    session =
      statement () sqlGetUserEmailsSubscribedToPollCreatedEvents

doPutUserByGitHubUserName ::
     ( Carrier sig m
     , Member DB sig
     )
  => GitHubUserName
  -> Maybe Text
  -> m (Entity User)
doPutUserByGitHubUserName name email =
  runDB session >>= \case
    Left err ->
      -- TODO deal with Hasql.Pool.UsageError how?
      error (show err)

    Right user ->
      pure user

  where
    session :: Session (Entity User)
    session =
      transaction Serializable Write $
        Transaction.statement name sqlGetUserByGitHubUserName >>= \case
          Nothing -> do
            userId :: UserId <-
              Transaction.statement (email, Just name) sqlPutUser

            pure
              (Entity userId User
                { email = email
                , gitHub = Just name
                , subscribedToPollCreated = False
                })

          Just user ->
            pure user

doSetUserSubscription ::
     ( Carrier sig m
     , Member DB sig
     )
  => UserId
  -> Subscription
  -> m ()
doSetUserSubscription userId sub =
  runDB session >>= \case
    Left err ->
      error (show err)

    Right () ->
      pure ()

  where
    session :: Session ()
    session =
      statement (userId, sub) sqlSetUserSubscription

runPersistUserDB :: PersistUserCarrierDB m a -> m a
runPersistUserDB (PersistUserCarrierDB m) =
  m


--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

sqlGetUserByGitHubUserName :: Statement GitHubUserName (Maybe (Entity User))
sqlGetUserByGitHubUserName =
  Statement sql encoder decoder True

  where
    sql :: ByteString
    sql =
      "SELECT id, email, $1, subscribed_to_poll_created FROM users WHERE github = $1"

    encoder :: Encoder.Params GitHubUserName
    encoder =
      Encoder.param gitHubUserNameEncoder

    decoder :: Decoder.Result (Maybe (Entity User))
    decoder =
      Decoder.rowMaybe
        (Entity
          <$> Decoder.column userIdDecoder
          <*> userDecoder)

sqlGetUserEmailsSubscribedToPollCreatedEvents :: Statement () [Text]
sqlGetUserEmailsSubscribedToPollCreatedEvents =
  Statement sql Encoder.unit decoder True

  where
    sql :: ByteString
    sql =
      "SELECT email FROM users WHERE subscribed_to_poll_created = true AND email IS NOT NULL"

    decoder :: Decoder.Result [Text]
    decoder =
      -- TODO Decoder.rowVector
      Decoder.rowList (Decoder.column Decoder.text)

sqlPutUser :: Statement (Maybe Text, Maybe GitHubUserName) UserId
sqlPutUser =
  Statement sql userEncoder decoder True

  where
    sql :: ByteString
    sql =
      "INSERT INTO users (email, github) VALUES ($1, $2) RETURNING id"

    decoder :: Decoder.Result UserId
    decoder =
      Decoder.singleRow (Decoder.column userIdDecoder)

sqlSetUserSubscription :: Statement (UserId, Subscription) ()
sqlSetUserSubscription =
  Statement sql encoder Decoder.unit True

  where
    sql :: ByteString
    sql =
      "UPDATE users SET subscribed_to_poll_created = $1 WHERE id = $2"

    encoder :: Encoder.Params (UserId, Subscription)
    encoder =
      fold
        [ view (_2 . #pollCreated) >$< Encoder.param Encoder.bool
        , view _1 >$< Encoder.param userIdEncoder
        ]
