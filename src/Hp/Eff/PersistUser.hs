module Hp.Eff.PersistUser
  ( PersistUserEffect(..)
  , getUserEmailsSubscribedToPollCreatedEvents
  , putUserByGitHubUserName
  ) where

import Hp.Eff.FirstOrder  (FirstOrderEffect(..))
import Hp.GitHub.UserName (GitHubUserName)
import Hp.User            (User)
import Hp.Entity (Entity)

import Control.Effect
import Control.Effect.Carrier


data PersistUserEffect (m :: Type -> Type) (k :: Type) where
  GetUserEmailsSubscribedToPollCreatedEvents ::
       ([Text] -> k)
    -> PersistUserEffect m k

  PutUserByGitHubUserName ::
       GitHubUserName
    -> Maybe Text
    -> (Entity User -> k)
    -> PersistUserEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect PersistUserEffect)

-- | Get all email addresses to blast with a "new poll was created" event.
getUserEmailsSubscribedToPollCreatedEvents ::
     ( Carrier sig m
     , Member PersistUserEffect sig
     )
  => m [Text]
getUserEmailsSubscribedToPollCreatedEvents =
  send (GetUserEmailsSubscribedToPollCreatedEvents pure)


-- | Insert and return a user, given its GitHub user name. If the user already
-- exists, just returns it.
putUserByGitHubUserName ::
     ( Carrier sig m
     , Member PersistUserEffect sig
     )
  => GitHubUserName
  -> Maybe Text
  -> m (Entity User)
putUserByGitHubUserName name email =
  send (PutUserByGitHubUserName name email pure)
