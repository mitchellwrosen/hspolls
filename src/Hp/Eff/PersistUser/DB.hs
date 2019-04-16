-- | Real database carrier for the PersistUser effect.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistUser.DB
  ( runPersistUserDB
  ) where

import Hp.Eff.DB          (DB, runDB)
import Hp.Eff.PersistUser
import Hp.Entity          (Entity(..))
import Hp.Entity.User     (User(..), UserId, userIdDecoder, userIdEncoder)
import Hp.GitHub.User     (GitHubUser(..))
import Hp.GitHub.UserName (gitHubUserNameDecoder, gitHubUserNameEncoder)
import Hp.Hasql           (statement, transaction)
import Hp.Subscription    (Subscription(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Prelude                hiding (id)

import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder


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
    L (GetUserById userId next) ->
      PersistUserCarrierDB (doGetUserById userId) >>= next

    L (GetUserEmailsSubscribedToPollCreatedEvents next) ->
      PersistUserCarrierDB doGetUserEmailsSubscribedToPollCreatedEvents >>= next

    L (PutUserByGitHubUser user next) ->
      PersistUserCarrierDB (doPutUserByGitHubUser user) >>= next

    L (SetUserSubscription userId sub next) -> do
      PersistUserCarrierDB (doSetUserSubscription userId sub)
      next

    R other ->
      PersistUserCarrierDB (eff (handleCoercible other))

doGetUserById ::
     ( Carrier sig m
     , Member DB sig
     )
  => UserId
  -> m (Maybe (Entity User))
doGetUserById userId =
  runDB $
    statement
      "SELECT email, gitHub, subscribed_to_poll_created FROM users WHERE id = $1"
      userId
      (Encoder.param userIdEncoder)
      (Decoder.rowMaybe
        (Entity userId
          <$> (do
                email <- Decoder.nullableColumn Decoder.text
                gitHub <- Decoder.nullableColumn gitHubUserNameDecoder
                subscribedToPollCreated <- Decoder.column Decoder.bool
                pure User
                  { email = email
                  , gitHub = gitHub
                  , subscribedToPollCreated = subscribedToPollCreated
                  })))

doGetUserEmailsSubscribedToPollCreatedEvents ::
     ( Carrier sig m
     , Member DB sig
     )
  => m [Text]
doGetUserEmailsSubscribedToPollCreatedEvents =
  runDB $
    statement
      "SELECT email FROM users WHERE subscribed_to_poll_created = true AND email IS NOT NULL"
      ()
      Encoder.unit
      -- TODO Decoder.rowVector
      (Decoder.rowList (Decoder.column Decoder.text))


doPutUserByGitHubUser ::
     ( Carrier sig m
     , Member DB sig
     )
  => GitHubUser
  -> m (Entity User)
doPutUserByGitHubUser GitHubUser { email, login } =
  runDB $
    transaction $ do
      result :: Maybe (Entity User) <-
        statement
          "SELECT id, email, subscribed_to_poll_created FROM users WHERE github = $1"
          login
          (Encoder.param gitHubUserNameEncoder)
          (Decoder.rowMaybe
            (Entity
              <$> Decoder.column userIdDecoder
              <*> (do
                    email <- Decoder.nullableColumn Decoder.text
                    subscribedToPollCreated <- Decoder.column Decoder.bool
                    pure User
                      { email = email
                      , gitHub = Just login
                      , subscribedToPollCreated = subscribedToPollCreated
                      })))

      case result of
        Nothing -> do
          userId :: UserId <-
            statement
              "INSERT INTO users (email, github) VALUES ($1, $2) RETURNING id"
              (email, Just login)
              (fold
                [ view _1 >$< Encoder.nullableParam Encoder.text
                , view _2 >$< Encoder.nullableParam gitHubUserNameEncoder
                ])
              (Decoder.singleRow (Decoder.column userIdDecoder))

          pure
            (Entity userId User
              { email = email
              , gitHub = Just login
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
  runDB $
    statement
      "UPDATE users SET subscribed_to_poll_created = $1 WHERE id = $2"
      (userId, sub)
      (fold
        [ view (_2 . #pollCreated) >$< Encoder.param Encoder.bool
        , view _1 >$< Encoder.param userIdEncoder
        ])
      Decoder.unit

runPersistUserDB :: PersistUserCarrierDB m a -> m a
runPersistUserDB (PersistUserCarrierDB m) =
  m

