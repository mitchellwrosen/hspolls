-- | Real database carrier for the PersistUser effect.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistUser.DB
  ( runPersistUserDB
  ) where

import Hp.Eff.DB          (DB, runDB)
import Hp.Eff.PersistUser
import Hp.Entity          (Entity(..))
import Hp.GitHub.UserName (GitHubUserName(..), gitHubUserNameEncoder)
import Hp.User            (User(..), userDecoder, userEncoder)
import Hp.UserId          (UserId(..), userIdDecoder)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Hasql.Session              (Session)
import Hasql.Statement            (Statement(..))
import Hasql.Transaction          (statement)
import Hasql.Transaction.Sessions (IsolationLevel(..), Mode(..), transaction)
import Prelude                    hiding (id)

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
    L (GetUserEmailsSubscribedToPollCreatedEvents next) ->
      PersistUserCarrierDB doGetUserEmailsSubscribedToPollCreatedEvents >>= next

    L (PutUserByGitHubUserName name next) ->
      PersistUserCarrierDB (doPutUserByGitHubUserName name) >>= next

    R other ->
      PersistUserCarrierDB (eff (handleCoercible other))

doGetUserEmailsSubscribedToPollCreatedEvents ::
     ( Carrier sig m
     , Member DB sig
     )
  => m [Text]
doGetUserEmailsSubscribedToPollCreatedEvents =
  pure ["frank@example.com"] -- TODO implement this

doPutUserByGitHubUserName ::
     ( Carrier sig m
     , Member DB sig
     )
  => GitHubUserName
  -> m (Entity User)
doPutUserByGitHubUserName name =
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
        statement name sqlGetUserByGitHubUserName >>= \case
          Nothing -> do
            let
              user :: User
              user =
                User
                  { gitHub = Just name
                  }

            userId :: UserId <-
              statement user sqlPutUser

            pure (Entity userId user)

          Just user ->
            pure user

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
      "SELECT id, github FROM users WHERE github = $1"

    encoder :: Encoder.Params GitHubUserName
    encoder =
      Encoder.param gitHubUserNameEncoder

    decoder :: Decoder.Result (Maybe (Entity User))
    decoder =
      Decoder.rowMaybe
        (Entity
          <$> Decoder.column userIdDecoder
          <*> userDecoder)

sqlPutUser :: Statement User UserId
sqlPutUser =
  Statement sql encoder decoder True

  where
    sql :: ByteString
    sql =
      "INSERT INTO users (github) VALUES ($1) RETURNING id"

    encoder :: Encoder.Params User
    encoder =
      userEncoder

    decoder :: Decoder.Result UserId
    decoder =
      Decoder.singleRow (Decoder.column userIdDecoder)
