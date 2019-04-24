module Hp.Eff.PersistUser
  ( PersistUserEffect(..)
  , getUserById
  , getUserEmailsSubscribedToPollCreatedEvents
  , putUserByGitHubUser
  , setUserSubscription
  ) where

import Hp.Eff.FirstOrder  (FirstOrderEffect(..))
import Hp.Entity          (Entity)
import Hp.Entity.User     (User, UserId)
import Hp.GitHub.User     (GitHubUser(..))
import Hp.Subscription    (Subscription)

import Control.Effect
import Control.Effect.Carrier


data PersistUserEffect (m :: Type -> Type) (k :: Type) where
  GetUserById ::
       UserId
    -> (Maybe (Entity User) -> k)
    -> PersistUserEffect m k

  GetUserEmailsSubscribedToPollCreatedEvents ::
       (Vector Text -> k)
    -> PersistUserEffect m k

  PutUserByGitHubUser ::
       GitHubUser
    -> (Entity User -> k)
    -> PersistUserEffect m k

  SetUserSubscription ::
       UserId
    -> Subscription
    -> k
    -> PersistUserEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect PersistUserEffect)

getUserById ::
     ( Carrier sig m
     , Member PersistUserEffect sig
     )
  => UserId
  -> m (Maybe (Entity User))
getUserById userId =
  send (GetUserById userId pure)

-- | Get all email addresses to blast with a "new poll was created" event.
getUserEmailsSubscribedToPollCreatedEvents ::
     ( Carrier sig m
     , Member PersistUserEffect sig
     )
  => m (Vector Text)
getUserEmailsSubscribedToPollCreatedEvents =
  send (GetUserEmailsSubscribedToPollCreatedEvents pure)


-- | Insert and return a user, given its GitHub user name and email address. If
-- the user already exists, just returns it (updating email address if
-- necessary).
putUserByGitHubUser ::
     ( Carrier sig m
     , Member PersistUserEffect sig
     )
  => GitHubUser
  -> m (Entity User)
putUserByGitHubUser user =
  send (PutUserByGitHubUser user pure)

-- | Set the user's email subscription settings.
setUserSubscription ::
     ( Carrier sig m
     , Member PersistUserEffect sig
     )
  => UserId
  -> Subscription
  -> m ()
setUserSubscription userId sub =
  send (SetUserSubscription userId sub (pure ()))
