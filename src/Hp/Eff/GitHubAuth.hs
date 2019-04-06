module Hp.Eff.GitHubAuth
  ( GitHubAuthEffect(..)
  , gitHubAuth
  , GitHubAuthCarrier(..)
  ) where

import Hp.Eff.FirstOrder (FirstOrderEffect(..))
import Hp.GitHub.Code    (GitHubCode)
import Hp.GitHub.User    (GitHubUser)

import Control.Effect
import Control.Effect.Carrier
import GHC.TypeLits           (Symbol)


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

newtype GitHubAuthCarrier
          (name :: Symbol)
          (env :: Type)
          (m :: Type -> Type)
          (a :: Type)
  = GitHubAuthCarrier (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)
