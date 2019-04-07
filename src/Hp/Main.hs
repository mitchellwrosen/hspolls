{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Hp.Main where

import Hp.API
import Hp.Eff.DB              (runDBC)
import Hp.Eff.GitHubAuth      (GitHubAuthEffect, gitHubAuth)
import Hp.Eff.GitHubAuth.Http (runGitHubAuthHttp)
import Hp.Eff.HttpClient      (runHttpManager)
import Hp.Eff.ManagePoll      (ManagePoll, ManagePollDBC(..), savePoll)
import Hp.Env
import Hp.GitHub.ClientId     (GitHubClientId(..))
import Hp.GitHub.ClientSecret (GitHubClientSecret(..))
import Hp.GitHub.Code         (GitHubCode)
import Hp.Poll
import Hp.PostgresConfig      (PostgresConfig, acquirePostgresPool)

import Control.Effect
-- import Control.Effect.Error
import Control.Effect.Reader
-- import Control.Monad.Trans.Except (ExceptT(..))
import System.IO (readFile)

import qualified Data.ByteString             as ByteString
import qualified Dhall                       as Dhall
import qualified Network.HTTP.Client         as Http
import qualified Network.HTTP.Client.TLS     as Http (tlsManagerSettings)
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Servant                     as Servant
import qualified Servant.Client              as Servant
import qualified Servant.Server.Generic      as Servant
import qualified Text.Blaze.Html             as Blaze
import qualified Text.Blaze.Html5            as Blaze
import qualified Text.Blaze.Html5.Attributes as Blaze


main :: IO ()
main = do
  gitHubClientSecret :: [Char] <-
    readFile "github-client-secret" <|> pure "0xDEADBEEF"

  pgConfig :: PostgresConfig <- Dhall.input Dhall.auto "./pg.dhall"

  pgPool <- acquirePostgresPool pgConfig

  putStrLn "Running on port 8000"
  putStrLn ("Using GitHub client secret: " ++ gitHubClientSecret)

  httpManager :: Http.Manager <-
    Http.newManager Http.tlsManagerSettings

  let
    env :: Env
    env =
      Env
        { httpManager = httpManager
          -- TODO don't hard code client id even though it's not a secret
        , gitHubClientId = GitHubClientId "0708940f1632f7a953e8"
        , gitHubClientSecret = GitHubClientSecret (gitHubClientSecret ^. packed)
        , postgresPool = pgPool
        }

  Warp.run 8000 (application env)

application ::
     Env
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
application env =
  Servant.genericServeTWithContext
    η
    API
      { getLoginRoute = handleGetLogin
      , getLoginGitHubRoute = handleGetLoginGitHub
      , postPollRoute = handlePostPoll
      }
    Servant.EmptyContext
  where
    η :: ∀ a. _ a -> Servant.Handler a
    η = runGitHubAuthHttp @Env
      >>> runHttpManager @Env
      >>> unManagePollDBC
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

handleGetLogin ::
     ( Carrier sig m
     )
  => m Blaze.Html
handleGetLogin =
  -- TODO set state get param
  -- TODO set redirect_uri get param
  pure $
    Blaze.a "Log in with GitHub" Blaze.!
      Blaze.href
        (Blaze.unsafeByteStringValue
          (ByteString.concat
            [ "https://github.com/login/oauth/authorize?"
            , "allow_signup=false&"
            , "client_id=0708940f1632f7a953e8"
            ]))

handleGetLoginGitHub ::
     ∀ m sig.
     ( Carrier sig m
     , Member GitHubAuthEffect sig
     )
  => GitHubCode
  -> m Blaze.Html
handleGetLoginGitHub code =
  gitHubAuth code >>= \case
    Nothing ->
      pure "Couldn't auth"

    Just user ->
      pure (Blaze.toHtml (show user))

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
