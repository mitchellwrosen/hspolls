-- | Hasql helpers.

module Hp.Hasql
  ( statement
  , transaction
  ) where

import Hasql.Decoders             (Result)
import Hasql.Encoders             (Params)
import Hasql.Session              (Session)
import Hasql.Transaction.Sessions (IsolationLevel(..), Mode(..))

import qualified Hasql.Session              (statement)
import qualified Hasql.Statement
import qualified Hasql.Transaction          (session)
import qualified Hasql.Transaction.Sessions as Hasql.Transaction (transaction)


statement ::
     ByteString
  -> a
  -> Params a
  -> Result b
  -> Session b
statement sql params encoder decoder =
  Hasql.Session.statement
    params
    (Hasql.Statement.Statement sql encoder decoder True)

transaction ::
     Session a
  -> Session a
transaction session =
  Hasql.Transaction.transaction
    Serializable
    Write
    (Hasql.Transaction.session session)

