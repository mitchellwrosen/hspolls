module App.Prelude
  ( module Export
  , undefined
  ) where

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Export

import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Export

import Data.Either (Either(..), choose, either, fromLeft, fromRight, hush, isLeft, isRight, note, note') as Export

import Halogen (Component, ComponentHTML, modify, modify_) as Export
import Halogen.HTML (HTML) as Export

import Effect.Class (class MonadEffect, liftEffect) as Export
import Effect.Ref (Ref) as Export
import Effect.Console (log) as Export

import Data.Newtype (class Newtype, ala, alaF, unwrap, wrap) as Export

import Data.Lens.Types (class Wander, AGrate, AGrate', ALens, ALens', APrism, APrism', AnIndexedLens, AnIndexedLens', AnIso, AnIso', Exchange(..), Fold, Fold', Forget(..), Getter, Getter', Grate, Grate', Grating, Indexed(..), IndexedFold, IndexedFold', IndexedGetter, IndexedGetter', IndexedLens, IndexedLens', IndexedOptic, IndexedOptic', IndexedSetter, IndexedSetter', IndexedTraversal, IndexedTraversal', Iso, Iso', Lens, Lens', Market(..), Optic, Optic', Prism, Prism', Re(..), Review, Review', Setter, Setter', Shop(..), Tagged(..), Traversal, Traversal', wander) as Export

import Data.Bounded (class Bounded, bottom, top) as Export
import Data.Enum (class Enum, pred, succ, upFromIncluding) as Export
import Data.Lens (_1, _2, _Just, _Left, _Nothing, _Right, first, left, right, second, united, over, (%~), set, (.~), view, (^.), toListOf, (^..), preview, (^?), traverseOf) as Export
import Data.Lens.Iso.Newtype (_Newtype) as Export
import Data.Lens.Record (prop) as Export
import Data.Symbol (SProxy(..)) as Export
import Data.Generic.Rep (class Generic) as Export
import Data.Generic.Rep.Show (genericShow) as Export
import Data.Generic.Rep.Enum (genericPred, genericSucc) as Export
import Data.Generic.Rep.Bounded (genericBottom, genericTop) as Export

import Data.Foldable (class Foldable, intercalate) as Export

import Control.Monad.Error.Class (class MonadThrow, try) as Export
import Control.Monad.Trans.Class (lift) as Export
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT) as Export
import Control.Monad.Except (class MonadError, ExceptT(..), runExceptT, except) as Export
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), runReaderT, ask, asks) as Export
import Data.Const (Const) as Export
import Control.Alt as Export
import Effect.Aff (Aff, launchAff, forkAff, throwError, catchError) as Export
import Effect.Aff.Class (class MonadAff, liftAff) as Export
import Effect (Effect) as Export
import Effect.Exception (Error, error) as Export

import Data.Symbol (class IsSymbol) as Export
import Prim.Row (class Cons) as Export
import Data.Date (Date) as Export
import Data.NonEmpty (NonEmpty(..), (:|)) as Export

import App.Data.Route (Route(..)) as Export
import App.Effect.Navigate (class Navigate, navigate) as Export

import Unsafe.Coerce (unsafeCoerce)

import Prelude

undefined :: ∀ a. a
undefined = unsafeCoerce unit
