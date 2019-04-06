{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Hp.Main where

import Hp.API
import Hp.Eff.HttpClient (HttpClient, runHttpManager)
import Hp.Env
import Hp.Form

import qualified Hp.GitHub
import qualified Hp.GitHub.AccessToken as Hp.GitHub (AccessToken)

import Control.Effect
import Control.Effect.Error
import Control.Effect.Reader
import Control.Monad.Trans.Except (ExceptT(..))

import qualified Data.ByteString             as ByteString
import qualified Network.HTTP.Client         as Http
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
  httpManager :: Http.Manager <-
    Http.newManager Http.defaultManagerSettings

  Warp.run 8000 (application httpManager)

application ::
     Http.Manager
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
application httpManager =
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
    η = runHttpManager @Env
      >>> runReader (Env httpManager)
      >>> runError @Servant.ClientError
      >>> runM @IO
      >>> over (mapped . _Left) toServerError
      >>> ExceptT
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
    Blaze.a mempty Blaze.!
      Blaze.href
        (Blaze.unsafeByteStringValue
          (ByteString.concat
            [ "https://github.com/login/oauth/authorize?"
            , "allow_signup=false&"
            , "client_id=0708940f1632f7a953e8"
            ]))

handleGetLoginGitHub ::
     ( Carrier sig m
     , Member (Error Servant.ClientError) sig
     , Member HttpClient sig
     )
  => Text
  -> m Servant.NoContent
handleGetLoginGitHub code = do
  accessToken :: Hp.GitHub.AccessToken <-
    Hp.GitHub.postLoginOauthAccessToken
      Hp.GitHub.clientId
      clientSecret
      code
      redirectUri
      state

  -- TODO finish oauth flow

  pure Servant.NoContent

  where
    clientSecret :: Text
    clientSecret =
      "" -- TODO client secret

    redirectUri :: Maybe Text
    redirectUri =
      -- TODO type safe link
      Just "/login/github"

    state :: Maybe Text
    state =
      -- TODO send random state
      Nothing

handlePostPoll ::
     ( Carrier sig m
     )
  => Form
  -> m Servant.NoContent
handlePostPoll _form =
  pure Servant.NoContent
