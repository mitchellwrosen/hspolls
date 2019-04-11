{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Hp.Main where

import Hp.API
import Hp.Config                      (Config(..), prettyPrintConfig,
                                       readConfigFile)
import Hp.Eff.Await.Chan              (runAwaitChan)
import Hp.Eff.DB                      (runDBC)
import Hp.Eff.GitHubAuth.Http         (runGitHubAuthHttp)
import Hp.Eff.HttpRequest.IO          (runHttpRequestIO)
import Hp.Eff.HttpSession.IO          (runHttpSessionIO)
import Hp.Eff.Log.Stdout              (runLogStdout)
import Hp.Eff.PersistPoll.DB          (PersistPollDBC(..))
import Hp.Eff.PersistPollAnswer.DB    (runPersistPollAnswerDB)
import Hp.Eff.PersistUser.DB          (runPersistUserDB)
import Hp.Eff.SendEmail.AmazonSES     (runSendEmailAmazonSES)
import Hp.Eff.Yield.Chan              (runYieldChan)
import Hp.Email                       (Email)
import Hp.Event.PollAnswered          (PollAnsweredEvent)
import Hp.Event.PollCreated           (PollCreatedEvent)
import Hp.GitHub.ClientId             (GitHubClientId)
import Hp.GitHub.ClientSecret         (GitHubClientSecret)
import Hp.Handler.AnswerPoll          (handleAnswerPoll)
import Hp.Handler.CreatePoll          (handleCreatePoll)
import Hp.Handler.GetMetrics          (handleGetMetrics)
import Hp.Handler.GetRoot             (handleGetRoot)
import Hp.Handler.GetUserProfile      (handleGetUserProfile)
import Hp.Handler.GitHubOauthCallback (handleGitHubOauthCallback)
import Hp.Metrics                     (requestCounter)
import Hp.PostgresConfig              (acquirePostgresPool)
import Hp.TBroadcastChan
import Hp.Worker.SendEmail            (sendEmailWorker)
import Hp.Worker.SendPollCreatedEmail (sendPollCreatedEmailWorker)

import Control.Concurrent.STM
import Control.Effect
import Control.Effect.Error       (runError)
import Control.Monad.Trans.Except (ExceptT(..))
import Servant                    (Context((:.)))
import Servant.Auth.Server        (CookieSettings, JWTSettings)
import System.Exit                (exitFailure)

import qualified Data.Text.IO             as Text
import qualified Hasql.Pool               as Hasql (Pool)
import qualified Network.AWS              as Aws
import qualified Network.AWS.Env          as Aws (newEnvWith)
import qualified Network.HTTP.Client      as Http
import qualified Network.HTTP.Client.TLS  as Http (tlsManagerSettings)
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Prometheus
import qualified Servant
import qualified Servant.Server.Generic   as Servant (genericServeTWithContext)
import qualified SlaveThread


main :: IO ()
main = do
  config :: Config <-
    readConfigFile "./etc/config.dhall" >>= \case
      Left errs -> do
        for_ errs Text.putStrLn
        exitFailure

      Right config ->
        pure config

  prettyPrintConfig config

  httpManager :: Http.Manager <-
    Http.newManager Http.tlsManagerSettings

  awsEnv :: Aws.Env <-
    Aws.newEnvWith
      (Aws.FromKeys
        (config ^. #aws . #accessKeyId)
        (config ^. #aws . #secretAccessKey))
      Nothing
      httpManager

  jwtSettings :: JWTSettings <-
    either id pure (config ^. #session . #jwt)

  pgPool :: Hasql.Pool <-
    acquirePostgresPool (config ^. #postgres)

  pollAnsweredEventChan :: TBroadcastChan PollAnsweredEvent <-
    newTBroadcastChanIO

  pollCreatedEventChan :: TBroadcastChan PollCreatedEvent <-
    newTBroadcastChanIO

  emailChan :: TBroadcastChan Email <-
    newTBroadcastChanIO

  void . SlaveThread.fork $ do
    chan :: TChan PollCreatedEvent <-
      dupTBroadcastChanIO pollCreatedEventChan

    sendPollCreatedEmailWorker
      & runAwaitChan chan
      & runPersistUserDB
      & runDBC pgPool
      & runYieldChan (unsafeTBroadcastChanToTChan emailChan)
      & runM

  void . SlaveThread.fork $ do
    chan :: TChan Email <-
      dupTBroadcastChanIO emailChan

    sendEmailWorker
      & runAwaitChan chan
      & runSendEmailAmazonSES awsEnv
      & runLogStdout
      & runM

  Warp.run
    (fromIntegral (config ^. #port))
    (middleware
      (application
        (config ^. #session . #cookie)
        (config ^. #gitHub . #clientId)
        (config ^. #gitHub . #clientSecret)
        httpManager
        jwtSettings
        pgPool
        pollAnsweredEventChan
        pollCreatedEventChan))

middleware ::
     (  Wai.Request
     -> (Wai.Response -> IO Wai.ResponseReceived)
     -> IO Wai.ResponseReceived)
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
middleware app request respond = do
  Prometheus.incCounter requestCounter
  app request respond

application ::
     CookieSettings
  -> GitHubClientId
  -> GitHubClientSecret
  -> Http.Manager
  -> JWTSettings
  -> Hasql.Pool
  -> TBroadcastChan PollAnsweredEvent
  -> TBroadcastChan PollCreatedEvent
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
application
    cookieSettings
    gitHubClientId
    gitHubClientSecret
    httpManager
    jwtSettings
    postgresPool
    pollAnsweredEventChan
    pollCreatedEventChan = do

  Servant.genericServeTWithContext
    η
    API
      { answerPollRoute = handleAnswerPoll
      , createPollRoute = handleCreatePoll
      , getRootRoute = handleGetRoot
      , getMetricsRoute = handleGetMetrics
      , getUserProfileRoute = handleGetUserProfile
      , gitHubOauthCallbackRoute = handleGitHubOauthCallback
      }
    (cookieSettings
      :. jwtSettings
      :. Servant.EmptyContext)

  where
    η :: ∀ a. _ a -> Servant.Handler a
    η =   -- Outgoing HTTP requests
          runGitHubAuthHttp gitHubClientId gitHubClientSecret
      >>> runHttpRequestIO httpManager

          -- Persistence layer
      >>> unPersistPollDBC
      >>> runPersistPollAnswerDB
      >>> runPersistUserDB
      >>> runDBC postgresPool

          -- HTTP session
      >>> runHttpSessionIO cookieSettings jwtSettings

          -- Event handlers
      >>> runYieldChan (unsafeTBroadcastChanToTChan pollAnsweredEventChan)
      >>> runYieldChan (unsafeTBroadcastChanToTChan pollCreatedEventChan)

          -- Error handlers
      >>> runError @Servant.ServerError

          -- IO boilerplate
      >>> runM @IO
      >>> ExceptT
      >>> Servant.Handler
