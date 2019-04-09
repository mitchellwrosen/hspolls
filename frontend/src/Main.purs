module Main where

import App.Prelude

import App.AppM (Env, runAppM)
import App.Data.Route (routeCodec, Route(Home))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import App.Component.Router as Router
import Routing.Duplex (parse)
import Routing.PushState (matchesWith, makeInterface)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  env <- makeEnv
  driver <- runUI (H.hoist (runAppM env) Router.component) Home body

  liftEffect do
    -- hook up router
    void $ env.nav # matchesWith (parse routeCodec) \old new ->
      when (old /= Just new) do
        launchAff_ $ driver.query $ Router.Navigate new unit

  pure unit

makeEnv :: Aff Env
makeEnv = liftEffect do
  nav <- makeInterface
  pure { baseUrl: "/api/"
       , nav: nav
       }
