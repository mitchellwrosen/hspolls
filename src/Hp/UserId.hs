module Hp.UserId
  ( UserId(..)
  , userIdDecoder
  ) where

import Data.UUID (UUID)

import qualified Hasql.Decoders as Decoder


newtype UserId
  = UserId { unUserId :: UUID }
  deriving stock (Show)

userIdDecoder :: Decoder.Value UserId
userIdDecoder =
  UserId <$> Decoder.uuid
