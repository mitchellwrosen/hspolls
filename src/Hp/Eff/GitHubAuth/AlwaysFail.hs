-- | GitHub auth carrier that always fails to authenticate.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.GitHubAuth.AlwaysFail
  ( runGitHubAuthAlwaysFail
  ) where

import Hp.Eff.GitHubAuth (GitHubAuthEffect(..))

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum

newtype GitHubAuthCarrierAlwaysFail m a
  = GitHubAuthCarrierAlwaysFail { unGitHubAuthCarrierAlwaysFail :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     )
  => Carrier (GitHubAuthEffect :+: sig) (GitHubAuthCarrierAlwaysFail m) where

  eff ::
       (GitHubAuthEffect :+: sig) (GitHubAuthCarrierAlwaysFail m) (GitHubAuthCarrierAlwaysFail m a)
    -> GitHubAuthCarrierAlwaysFail m a
  eff = \case
    L (GitHubAuth _code next) ->
      next Nothing

    R other ->
      GitHubAuthCarrierAlwaysFail (eff (handleCoercible other))

runGitHubAuthAlwaysFail ::
     GitHubAuthCarrierAlwaysFail m a
  -> m a
runGitHubAuthAlwaysFail =
  unGitHubAuthCarrierAlwaysFail
