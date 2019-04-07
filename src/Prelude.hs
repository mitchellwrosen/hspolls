module Prelude
  ( module X
  ) where

import Control.Applicative        as X ((<|>))
import Control.Category           as X ((>>>))
import Control.Exception.Safe     as X (SomeException, throwIO)
import Control.Lens               as X
  (Lens, Lens', Traversal, Traversal', Getter, Fold, _1, _2, _3, _4, _5, _head, _init, _Just,
    _last, _Left, _Nothing, _Right, _tail, at, each, folded, ix, mapped,
    over, preview, set, to, toListOf, view, (^.), (^..), (^?), (%~), (.~),
    foldMapOf, foldlOf, foldrOf, review, reviews, re)
import Data.Text.Lens             as X ( packed, unpacked, builder, text, _Text)
import Data.Text.Strict.Lens      as X ( utf8 )
import Data.ByteString.Lens       as X ( packedBytes, packedChars )
import Control.Monad.IO.Class     as X
import Data.ByteString            as X (ByteString)
import Data.Coerce                as X (Coercible, coerce)
import Data.Foldable              as X (asum)
import Data.Functor.Contravariant as X ( (>$<) )
import Data.Generics.Product      as X (HasType, field, typed)
import Data.Kind                  as X (Type)
import Data.Sequence              as X (Seq)
import Data.Text                  as X (Text)
import GHC.Generics               as X (Generic)
import PreludeFromBase            as X
