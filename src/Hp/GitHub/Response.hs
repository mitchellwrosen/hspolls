module Hp.GitHub.Response where

import Hp.GitHub.ErrorResponse (ErrorResponse)

import Data.Aeson       (FromJSON(..), Value)
import Data.Aeson.Types (Parser)


data Response a
  = ResponseError ErrorResponse
  | ResponseSuccess a
  deriving stock (Show)

instance FromJSON a => FromJSON (Response a) where
  parseJSON :: Value -> Parser (Response a)
  parseJSON value =
    asum
      [ ResponseError <$> parseJSON value
      , ResponseSuccess <$> parseJSON value
      ]
