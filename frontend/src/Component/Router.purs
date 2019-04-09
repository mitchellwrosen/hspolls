module App.Component.Router where

import App.Prelude

import App.Data.Route (Route(..))
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC

type State
  = { route :: Route
    }

data Query k
  = Navigate Route k

type ChildSlots =
  ()

component
  :: ∀ o m. Navigate m
     => MonadAff m
     => H.Component HH.HTML Query Route o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery
                                     }
    }

initialState :: Route -> State
initialState r = { route: r
                 }

render
  :: ∀ m. Navigate m
     => MonadAff m
     => State
     -> H.ComponentHTML Void ChildSlots m
render state =
  HH.div_
  [ child
  ]
  where
    child :: HH.ComponentHTML Void ChildSlots m
    child = case state.route of
      Home -> HH.h1_ [ HH.text "Home" ]

handleQuery ∷ ∀ a o m x.  MonadAff m => Query x -> H.HalogenM State a ChildSlots o m (Maybe x)
handleQuery = case _ of
  Navigate r k -> Just k <$ do
    H.modify_ (_ { route = r })
