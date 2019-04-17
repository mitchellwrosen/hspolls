{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Hp.Main
  ( main
  ) where

import Hp.API
import Hp.Config                      (Config(..), GitHubConfig(..),
                                       readConfigFile)
import Hp.Eff.Await.Chan              (runAwaitChan)
import Hp.Eff.DB                      (runDBC)
import Hp.Eff.GetCurrentTime          (GetCurrentTimeEffect(..))
import Hp.Eff.GitHubAuth.AlwaysFail   (runGitHubAuthAlwaysFail)
import Hp.Eff.GitHubAuth.Http         (runGitHubAuthHttp)
import Hp.Eff.HttpRequest.IO          (runHttpRequestIO)
import Hp.Eff.HttpSession.IO          (runHttpSessionIO)
import Hp.Eff.Log                     (log)
import Hp.Eff.Log.Stdout              (runLogStdout)
import Hp.Eff.PersistPoll.DB          (runPersistPollDB)
import Hp.Eff.PersistPollAnswer.DB    (runPersistPollAnswerDB)
import Hp.Eff.PersistUser.DB          (runPersistUserDB)
import Hp.Eff.SendEmail.AmazonSES     (runSendEmailAmazonSES)
import Hp.Eff.Throw                   (ThrowEffect(..), runThrow, throw)
import Hp.Eff.Yield.Chan              (runYieldChan)
import Hp.Email                       (Email)
import Hp.Event.PollAnswered          (PollAnsweredEvent)
import Hp.Event.PollCreated           (PollCreatedEvent)
import Hp.Handler.AnswerPoll          (handleAnswerPoll)
import Hp.Handler.CreatePoll          (handleCreatePoll)
import Hp.Handler.GetMetrics          (handleGetMetrics)
import Hp.Handler.GetPoll             (handleGetPoll)
import Hp.Handler.GetRoot             (handleGetRoot)
import Hp.Handler.GetUserProfile      (handleGetUserProfile)
import Hp.Handler.GitHubOauthCallback (handleGitHubOauthCallback)
import Hp.Handler.Subscribe           (handleSubscribe)
import Hp.Metrics                     (requestCounter)
import Hp.PostgresConfig              (acquirePostgresPool)
import Hp.TBroadcastChan
import Hp.Worker.SendEmail            (sendEmailWorker)
import Hp.Worker.SendPollCreatedEmail (sendPollCreatedEmailWorker)

import Control.Concurrent.STM
import Control.Effect
import Control.Effect.Interpret
import Control.Monad.Trans.Except (ExceptT(..))
import Servant                    (Context((:.)))
import Servant.Auth.Server        (CookieSettings, JWTSettings)
import System.Exit                (exitFailure)

import qualified Data.Text.IO             as Text
import qualified Data.Time                as Time (getCurrentTime)
import qualified Hasql.Pool               as Hasql (Pool, UsageError)
import qualified Network.AWS              as Aws
import qualified Network.AWS.Env          as Aws (newEnvWith)
import qualified Network.HTTP.Client      as Http
import qualified Network.HTTP.Client.TLS  as Http (tlsManagerSettings)
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Prometheus
import qualified Servant
import qualified Servant.Client           as Servant (ClientError)
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

    forever $ do
      result :: Either Hasql.UsageError Void <-
        sendPollCreatedEmailWorker
          & runAwaitChan chan
          & runPersistUserDB
          & runDBC pgPool
          & runThrow @Hasql.UsageError
          & runYieldChan (unsafeTBroadcastChanToTChan emailChan)
          & runM

      case result of
        Left err -> do
          log (show err ^. packed)
            & runLogStdout
            & runM

        Right void ->
          absurd void

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
        (config ^. #gitHub)
        httpManager
        jwtSettings
        pgPool
        pollAnsweredEventChan
        pollCreatedEventChan))

middleware ::
     (  Wai.Request
     -> (Wai.Response -> IO Wai.ResponseReceived)
     -> IO Wai.ResponseReceived
     )
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
middleware app request respond = do
  Prometheus.incCounter requestCounter
  app request respond

application ::
     CookieSettings
  -> Maybe GitHubConfig
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
    gitHubConfig
    httpManager
    jwtSettings
    postgresPool
    pollAnsweredEventChan
    pollCreatedEventChan = do

  Servant.genericServeTWithContext
    eta
    API
      { answerPollRoute = handleAnswerPoll
      , createPollRoute = handleCreatePoll
      , getMetricsRoute = handleGetMetrics
      , getPollRoute = handleGetPoll
      , getRootRoute = handleGetRoot
      , getUserProfileRoute = handleGetUserProfile
      , gitHubOauthCallbackRoute = handleGitHubOauthCallback
      , subscribeRoute = handleSubscribe
      }
    (cookieSettings
      :. jwtSettings
      :. Servant.EmptyContext)

  where
    eta :: forall a. _ a -> Servant.Handler a
    eta =   -- Outgoing HTTP requests
          runGitHubAuth
      >>> runHttpRequestIO httpManager

          -- Persistence layer
      >>> runPersistPollDB
      >>> runPersistPollAnswerDB
      >>> runPersistUserDB
      >>> runDBC postgresPool

          -- HTTP session
      >>> runHttpSessionIO cookieSettings jwtSettings

          -- Event handlers
      >>> runYieldChan (unsafeTBroadcastChanToTChan pollAnsweredEventChan)
      >>> runYieldChan (unsafeTBroadcastChanToTChan pollCreatedEventChan)

          -- Error handlers
      >>> runInterpret
            (\(Throw err) -> do
              log (show (err :: Hasql.UsageError) ^. packed)
              throw Servant.err500)
      >>> runInterpret
            (\(Throw err) -> do
              log (show (err :: Servant.ClientError) ^. packed)
              throw Servant.err500)
      >>> runThrow

          -- Current time
      >>> runInterpret
            (\(GetCurrentTime next) -> liftIO Time.getCurrentTime >>= next)

          -- Logging
      >>> runLogStdout

          -- IO boilerplate
      >>> runM
      >>> ExceptT
      >>> Servant.Handler

    runGitHubAuth :: _ -> _ -- GHC wants this?
    runGitHubAuth =
      case gitHubConfig of
        Nothing ->
          runGitHubAuthAlwaysFail
        Just GitHubConfig { clientId, clientSecret } ->
          runGitHubAuthHttp clientId clientSecret
