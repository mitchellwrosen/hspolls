module App.AppM where

import App.Prelude

import App.Data.Route as Route
import App.Effect.Navigate (class Navigate, navigate)
import Foreign (unsafeToForeign)
import Routing.Duplex (print)
import Routing.PushState (PushStateInterface)
import Type.Equality (class TypeEquals, from)

type Env =
  { baseUrl :: String
  , nav :: PushStateInterface
  }

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
  navigate r = do
    env <- ask
    liftEffect $ env.nav.pushState (unsafeToForeign {}) (print Route.routeCodec r)
