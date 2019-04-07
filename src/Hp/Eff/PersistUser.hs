module Hp.Eff.PersistUser
  ( PersistUserEffect(..)
  , putUserByGitHubUserName
  ) where

import Hp.Eff.FirstOrder  (FirstOrderEffect(..))
import Hp.GitHub.UserName (GitHubUserName)
import Hp.User            (User)
import Hp.UserId          (UserId)

import Control.Effect
import Control.Effect.Carrier


data PersistUserEffect (m :: Type -> Type) (k :: Type) where
  PutUserByGitHubUserName ::
       GitHubUserName
    -> (User UserId -> k)
    -> PersistUserEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect PersistUserEffect)

-- | Insert and return a user, given its GitHub user name. If the user already
-- exists, just returns it.
putUserByGitHubUserName ::
     ( Carrier sig m
     , Member PersistUserEffect sig
     )
  => GitHubUserName
  -> m (User UserId)
putUserByGitHubUserName name =
  send (PutUserByGitHubUserName name pure)
