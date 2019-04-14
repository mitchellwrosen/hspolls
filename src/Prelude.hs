module Prelude
  ( error
  , undefined
  , module X
  ) where

import Control.Applicative        as X ((<|>))
import Control.Category           as X ((>>>))
import Control.Exception.Safe     as X (SomeException, throwIO)
import Control.Lens               as X (Fold, Getter, Lens, Lens', Traversal,
                                        Traversal', at, each, foldMapOf, folded,
                                        foldlOf, foldrOf, ix, mapped, over,
                                        preview, re, review, reviews, set,
                                        strict, to, toListOf, view, (%~), (.~),
                                        (^.), (^..), (^?), (^?!), _1, _2, _3,
                                        _4, _5, _Just, _Left, _Nothing, _Right,
                                        _head, _init, _last, _tail)
import Control.Monad              as X (forever, guard, unless, when)
import Control.Monad.IO.Class     as X
import Data.ByteString            as X (ByteString)
import Data.ByteString.Lens       as X (packedBytes, packedChars)
import Data.Coerce                as X (Coercible, coerce)
import Data.Foldable              as X (asum, fold, for_)
import Data.Function              as X ((&))
import Data.Functor               as X (void)
import Data.Functor.Contravariant as X ((>$<))
import Data.Generics.Labels       as X ()
import Data.Generics.Product      as X (HasType, typed)
import Data.Kind                  as X (Type)
import Data.Sequence              as X (Seq)
import Data.Text                  as X (Text)
import Data.Text.Lens             as X (builder, packed, text, unpacked, _Text)
import Data.Text.Strict.Lens      as X (utf8)
import Data.Void                  as X (Void, absurd)
import Debug.Trace                as X (traceShowM)
import GHC.Generics               as X (Generic)
import Numeric.Natural            as X (Natural)
import PreludeFromBase            as X hiding (error, log, undefined)

import qualified PreludeFromBase

error :: [Char] -> a
error =
  PreludeFromBase.error
{-# WARNING error "error" #-}

undefined :: a
undefined =
  PreludeFromBase.undefined
{-# WARNING undefined "undefined" #-}
