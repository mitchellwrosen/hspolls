module Hp.Eff.GitHubAuth
  ( GitHubAuthEffect(..)
  , gitHubAuth
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.GitHub.Code    (GitHubCode)
import Hp.GitHub.User    (GitHubUser)

import Control.Effect
import Control.Effect.Carrier


data GitHubAuthEffect (m :: Type -> Type) (k :: Type) where
  GitHubAuth ::
       GitHubCode
    -> (Maybe GitHubUser -> k)
    -> GitHubAuthEffect m k

  deriving stock (Functor)
  deriving (Effect, HFunctor)
       via (FirstOrderEffect GitHubAuthEffect)

gitHubAuth ::
     ( Carrier sig m
     , Member GitHubAuthEffect sig
     )
  => GitHubCode
  -> m (Maybe GitHubUser)
gitHubAuth code =
  send (GitHubAuth code pure)
