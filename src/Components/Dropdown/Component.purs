module Ocelot.Components.Dropdown where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, seeks, store)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Renderless.State (updateStore)
import Select as Select

data Query pq item m a
  = HandleSelect (Select.Message pq item) a
  | SetItems (Array item) a
  | SetSelection (Maybe item) a
  | Receive (Input pq item m) a

type StateStore pq item m =
  Store (State item) (H.ComponentHTML (Query pq item m) (ChildSlots pq item m) m)

type State item =
  { selectedItem :: Maybe item
  , items :: Array item
  }

type Input pq item m =
  { selectedItem :: Maybe item
  , items :: Array item
  , render :: State item -> H.ComponentHTML (Query pq item m) (ChildSlots pq item m) m
  }

data Message pq item
  = Selected item
  | VisibilityChanged Select.Visibility
  | Emit (pq Unit)

type Slot pq item m = H.Slot (Query pq item m) (Message pq item)
type ChildSlots pq item m =
  ( select :: Select.Slot pq () item m Unit )

_select = SProxy :: SProxy "select"

component
  :: ∀ pq item m
   . MonadAff m
  => H.Component HH.HTML (Query pq item m) (Input pq item m) (Message pq item) m
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }

  where
    initialState :: Input pq item m -> StateStore pq item m
    initialState i = store i.render
      { selectedItem: i.selectedItem
      , items: i.items
      }

    eval
      :: Query pq item m
      ~> H.HalogenM
          (StateStore pq item m)
          (Query pq item m)
          (ChildSlots pq item m)
          (Message pq item)
          m
    eval = case _ of
      HandleSelect message a -> case message of
        Select.Selected item -> do
          _ <- H.query _select unit $ Select.setVisibility Select.Off
          H.modify_ $ seeks _ { selectedItem = Just item }
          H.raise $ Selected item
          pure a
        Select.Emit query -> H.raise (Emit query) $> a
        Select.VisibilityChanged vis -> H.raise (VisibilityChanged vis) $> a
        _ -> pure a
      SetItems items a -> do
        H.modify_ $ seeks _ { items = items }
        pure a
      SetSelection item a -> do
        H.modify_ $ seeks _ { selectedItem = item }
        pure a
      Receive input a -> do
        H.modify_ $ updateStore input.render identity
        pure a

