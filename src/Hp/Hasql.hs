-- | Hasql helpers.

module Hp.Hasql
  ( statement
  ) where

import Hasql.Decoders    (Result)
import Hasql.Encoders    (Params)
import Hasql.Transaction (Transaction)

import qualified Hasql.Statement
import qualified Hasql.Transaction (statement)


statement ::
     ByteString
  -> a
  -> Params a
  -> Result b
  -> Transaction b
statement sql params encoder decoder =
  Hasql.Transaction.statement
    params
    (Hasql.Statement.Statement sql encoder decoder True)
