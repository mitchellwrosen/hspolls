-- | GitHub auth carrier that always fails to authenticate.

module Hp.Eff.GitHubAuth.AlwaysFail
  ( runGitHubAuthAlwaysFail
  ) where

import Hp.Eff.GitHubAuth (GitHubAuthEffect(..))

import Control.Effect.Interpret

runGitHubAuthAlwaysFail ::
     InterpretC GitHubAuthEffect m a
  -> m a
runGitHubAuthAlwaysFail =
  runInterpret $ \case
    GitHubAuth _code next ->
      next Nothing
