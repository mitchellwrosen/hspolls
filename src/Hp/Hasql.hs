-- | Hasql helpers.

module Hp.Hasql
  ( Session(..)
  , statement
  , transaction
  ) where

import Control.Monad.Trans.Class  (lift)
import Hasql.Decoders             (Result)
import Hasql.Encoders             (Params)
import Hasql.Transaction.Sessions (IsolationLevel(..), Mode(..))

import qualified Hasql.Session              (Session, statement)
import qualified Hasql.Statement
import qualified Hasql.Transaction          (Transaction(..))
import qualified Hasql.Transaction.Sessions as Hasql.Transaction (transaction)


-- | A newtype over an hasql Session that does not have a MonadIO instance.
-- The constructor is considered internal and unsafe, don't use it.
newtype Session a
  = Session { unSession :: Hasql.Session.Session a }
  deriving newtype (Applicative, Functor, Monad)

statement ::
     ByteString
  -> a
  -> Params a
  -> Result b
  -> Session b
statement sql params encoder decoder =
  Session
    (Hasql.Session.statement
      params
      (Hasql.Statement.Statement sql encoder decoder True))

transaction ::
     Session a
  -> Session a
transaction (Session session) =
  Session
    (Hasql.Transaction.transaction
      Serializable
      Write
      (Hasql.Transaction.Transaction (lift session)))
