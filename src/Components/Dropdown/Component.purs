module Ocelot.Component.Dropdown where

import Prelude

import Data.Array (index, length)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Select as S

type AddedState item =
  ( selectedItem :: Maybe item
  , items :: Array item
  )

type Input item =
  { selectedItem :: Maybe item
  , items :: Array item
  }

data Action item
  = Receive (Input item)

data Query item a
  = SetItems (Array item) a
  | SetSelection (Maybe item) a

data Message item
  = SelectionChanged (Maybe item) (Maybe item)
  | VisibilityChanged S.Visibility

type SelfSlot item slots index = S.Slot (Query item) slots (Message item) index

component
  :: ∀ item slots m
   . MonadAff m
  => (S.State (AddedState item) -> S.ComponentHTML (Action item) slots m)
  -> S.Component (Query item) slots (Input item) (Message item) m
component render = S.component mkInput $ S.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleQuery = handleQuery
  , handleEvent = handleEvent
  , receive = Just <<< Receive
  }
  where
    mkInput :: (Input item) -> S.Input (AddedState item)
    mkInput { items, selectedItem } =
      { inputType: S.Toggle
      , search: Nothing
      , debounceTime: Nothing
      , getItemCount: \state -> length state.items
      -- labels from AddedState
      , items
      , selectedItem
      }

    handleAction :: (Action item) -> S.HalogenM (AddedState item) (Action item) slots (Message item) m Unit
    handleAction = case _ of
      Receive { items, selectedItem } -> do
        H.modify_ _ { items = items, selectedItem = selectedItem }
        pure unit

    handleQuery :: forall a. (Query item) a -> S.HalogenM (AddedState item) (Action item) slots (Message item) m (Maybe a)
    handleQuery = case _ of
      SetItems items a -> do
        H.modify_ _ { items = items, selectedItem = Nothing }
        pure (Just a)

      SetSelection selectedItem a -> do
        H.modify_ _ { selectedItem = selectedItem }
        pure (Just a)

    handleEvent :: S.Event -> S.HalogenM (AddedState item) (Action item) slots (Message item) m Unit
    handleEvent = case _ of
      S.Selected idx -> do
        st <- H.get
        let selectedItem = index st.items idx
        H.modify_ _ { selectedItem = selectedItem, visibility = S.Off }
        H.raise $ SelectionChanged st.selectedItem selectedItem

      S.VisibilityChanged visibility -> do
        H.raise $ VisibilityChanged visibility

      _ -> pure unit
