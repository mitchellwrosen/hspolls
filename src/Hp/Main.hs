{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Hp.Main where

import Hp.API
import Hp.Config                   (Config(..), prettyPrintConfig,
                                    readConfigFile)
import Hp.Eff.DB                   (runDBC)
import Hp.Eff.GitHubAuth.Http      (runGitHubAuthHttp)
import Hp.Eff.HttpClient           (runHttpManager)
import Hp.Eff.ManagePoll           (ManagePoll, ManagePollDBC(..), savePoll)
import Hp.Eff.PersistUser.DB       (runPersistUserDB)
import Hp.Env
import Hp.Handler.Login.GitHub.GET (handleGetLoginGitHub)
import Hp.Handler.Root.GET         (handleGetRoot)
import Hp.Poll
import Hp.PostgresConfig           (acquirePostgresPool)

import Control.Effect
-- import Control.Effect.Error
import Control.Effect.Reader
-- import Control.Monad.Trans.Except (ExceptT(..))
import Servant     (Context((:.)))
import System.Exit (exitFailure)

import qualified Data.Text.IO             as Text
import qualified Network.HTTP.Client      as Http
import qualified Network.HTTP.Client.TLS  as Http (tlsManagerSettings)
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import qualified Servant.Client           as Servant (ClientError)
import qualified Servant.Server.Generic   as Servant (genericServeTWithContext)

import Servant.Auth.Server as Servant


main :: IO ()
main = do
  config :: Config <-
    readConfigFile "./config.dhall" >>= \case
      Left errs -> do
        for_ errs Text.putStrLn
        exitFailure

      Right config ->
        pure config

  prettyPrintConfig config

  pgPool <- acquirePostgresPool (config ^. #postgres)

  httpManager :: Http.Manager <-
    Http.newManager Http.tlsManagerSettings

  jwtSettings :: JWTSettings <-
    either id pure (config ^. #session . #jwt)

  let
    env :: Env
    env =
      Env
        { cookieSettings = config ^. #session . #cookie
        , httpManager = httpManager
        , gitHubClientId = config ^. #gitHub . #clientId
        , gitHubClientSecret = config ^. #gitHub . #clientSecret
        , jwtSettings = jwtSettings
        , postgresPool = pgPool
        }

  Warp.run
    (fromIntegral (config ^. #port))
    (application env)

application ::
     Env
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
application env = do
  Servant.genericServeTWithContext
    η
    API
      { getRootRoute = handleGetRoot
      , getLoginGitHubRoute = handleGetLoginGitHub @Env
      , postPollRoute = handlePostPoll
      }
    ((env ^. #cookieSettings)
      :. (env ^. #jwtSettings)
      :. Servant.EmptyContext)
  where
    η :: ∀ a. _ a -> Servant.Handler a
    η = runGitHubAuthHttp @Env
      >>> runHttpManager @Env
      >>> unManagePollDBC
      >>> runPersistUserDB
      >>> runDBC @Env
      >>> runReader env
      -- >>> runError @Servant.ClientError
      >>> runM @IO
      -- >>> over (mapped . _Left) toServerError
      -- >>> ExceptT
      >>> liftIO
      >>> Servant.Handler


-- TODO Generalize to ApplicationException
-- TODO Implement toServerError
toServerError
  :: Servant.ClientError
  -> Servant.ServerError
toServerError = undefined

handlePostPoll ::
     ( Carrier sig m
     , Member ManagePoll sig
     , MonadIO m
     )
  => Poll
  -> m Servant.NoContent
handlePostPoll poll = do
  _ <- savePoll poll
  pure Servant.NoContent
