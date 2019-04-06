module Prelude
  ( module X
  ) where

import Control.Category       as X ((>>>))
import Control.Exception.Safe as X (SomeException, throwIO)
import Control.Lens           as X (Lens, Traversal, mapped, over, set, view,
                                    (%~), (.~), (^.), _Left, _Right)
import Control.Lens           as X ((^.))
import Control.Monad.IO.Class as X
import Data.Kind              as X (Type)
import Data.Sequence          as X (Seq)
import Data.Text              as X (Text)
import GHC.Generics           as X (Generic)
import PreludeFromBase        as X
