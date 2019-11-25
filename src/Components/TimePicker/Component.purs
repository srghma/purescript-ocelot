module Ocelot.Component.TimePicker where

import Prelude

import Data.Array (index, length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Time (Time)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Data.DateTime as ODT
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Setters as Setters

----------
-- Time Units

data TimeUnit
  = TimeUnit SelectableStatus SelectedStatus Time

data SelectableStatus
  = NotSelectable
  | Selectable

data SelectedStatus
  = NotSelected
  | Selected

-- data Query a
--   = HandleSelect (Select.Message Query TimeUnit) a
--   | TriggerFocus a
--   | Synchronize a

type Input = { selection :: Maybe Time }
type AddedState =
  ( selection :: Maybe Time
  , timeUnits :: Array TimeUnit
  )

data Action
  = Synchronize

data Query a
  = GetSelection (Maybe Time -> a)
  | SetSelection (Maybe Time) a

data Message
  = SelectionChanged (Maybe Time)
  | VisibilityChanged Select.Visibility
  | Searched String

type ChildSlots = ()
type SelfSlot index = Select.Slot Query ChildSlots Message index

dropdownClasses :: Array HH.ClassName
dropdownClasses = HH.ClassName <$>
  [ "max-h-80"
  , "w-full"
  , "overflow-y-scroll"
  , "pin-t"
  , "pin-l"
  , "bg-white"
  , "text-center"
  ]

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML (Select.Query Query ChildSlots) Input Message m
component = Select.component mkInput $ Select.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleQuery = handleQuery
  , handleEvent = handleEvent
  }
  where
    mkInput :: Input -> Select.Input AddedState
    mkInput { selection } =
      { inputType: Select.Text
      , search: Nothing
      , debounceTime: Nothing
      , getItemCount: \state -> length state.timeUnits
      -- labels from AddedState
      , timeUnits: generateTimes selection
      , selection
      }

    handleAction :: Action -> H.HalogenM (Select.State AddedState) (Select.Action Action) ChildSlots Message m Unit
    handleAction = case _ of
      Synchronize -> do
        { selection } <- H.get
        let timeUnits = generateTimes selection
        let update = case selection of
              Just time -> _ { search = ODT.formatTime time }
              otherwise -> identity
        H.modify_ (update <<< _ { timeUnits = timeUnits })

    handleQuery :: forall a. Query a -> H.HalogenM (Select.State AddedState) (Select.Action Action) ChildSlots Message m (Maybe a)
    handleQuery = case _ of
      GetSelection reply -> do
        selection <- H.gets _.selection
        pure (Just (reply selection))

      SetSelection selection a -> do
        H.modify_ _ { selection = selection }
        pure (Just a)

    handleEvent :: Select.Event
                -> H.HalogenM (Select.State AddedState) (Select.Action Action) ChildSlots Message m Unit
    handleEvent = case _ of
      Select.Selected idx -> do
        timeUnits <- H.gets _.timeUnits
        case index timeUnits idx of
          Nothing -> pure unit
          Just (TimeUnit _ _ time) -> do
            let justTime = Just time
            -- We'll want to select the item here, set its status, and raise
            -- a message about its selection.
            H.modify_ _ { selection = justTime }
            H.raise $ SelectionChanged $ justTime
            handleAction Synchronize
            -- pure Nothing

      Select.VisibilityChanged visibility -> do
        H.raise $ VisibilityChanged visibility

      Select.Searched str -> do
        pure unit

    render :: Select.State AddedState -> H.ComponentHTML (Select.Action Action) ChildSlots m
    render state =
      HH.div_ [ renderSearch, renderSelect state ]
      where
        -- The page element that will hold focus, capture key events, etcetera
        renderSearch =
          Input.input ( Setters.setInputProps [ HP.value state.search ] )

        renderSelect tst =
          HH.div
            [ css "relative" ]
            $ if tst.visibility == Select.On
              then [ renderTimes tst ]
              else [ ]

-- The overall container for the time dropdown
renderTimes :: forall m. MonadAff m => Select.State AddedState -> H.ComponentHTML (Select.Action Action) ChildSlots m
renderTimes tst =
  Layout.popover
    ( Setters.setContainerProps
      [ HP.classes dropdownClasses ]
    )
    ( mapWithIndex renderItem tst.timeUnits )

renderItem :: forall m. MonadAff m => Int -> TimeUnit -> H.ComponentHTML (Select.Action Action) ChildSlots m
renderItem index item =
  HH.div
  -- Here's the place to use info from the item to render it in different
  -- states.
  -- if highlightedIndex == Just index then 'highlight' else 'dont'
    ( maybeSetItemProps index item
      [ css
        $ trim
        $ "relative p-3 transition-1/4 "
          <> (getTimeStyles item)
      ]
    )
    -- printDay will format our item correctly
    [ HH.text $ printTime item ]
  where
    -- If the timeunit is selectable,
    -- then augment the props with the correct click events.
    -- if not, then just don't provide the props at all.
    -- this is an easy way to "disable" functionality in the calendar.
    maybeSetItemProps i (TimeUnit Selectable _ _) props =
      Setters.setItemProps i props
    maybeSetItemProps _ _ props = props

    -- Get the correct styles for a time unit, dependent on its statuses
    getTimeStyles :: TimeUnit -> String
    getTimeStyles i
      = trim $ getSelectableStyles i
      <> " " <> getSelectedStyles i
      where
        getSelectableStyles :: TimeUnit -> String
        getSelectableStyles (TimeUnit NotSelectable _ _) =
          mempty
        getSelectableStyles _ =
          "cursor-pointer hover:bg-grey-97"

        getSelectedStyles :: TimeUnit -> String
        getSelectedStyles (TimeUnit _ Selected _) =
          "text-blue-88"
        getSelectedStyles _ =
          mempty

    -- Just a simple helper to format our TimeUnit into a day
    -- we can print out
    printTime :: TimeUnit -> String
    printTime (TimeUnit _ _ t) = ODT.formatTime t


----------
-- Other helpers for the file

-- Generate a standard set of time intervals.
generateTimes
  :: Maybe Time
  -> Array TimeUnit
generateTimes selection =
  ODT.defaultTimeRange <#> (generateTimeUnit selection)

generateTimeUnit
  :: Maybe Time
  -> Time
  -> TimeUnit
generateTimeUnit Nothing i =
  TimeUnit Selectable NotSelected i
generateTimeUnit (Just t) i
  | t == i = TimeUnit Selectable Selected i
  | otherwise = TimeUnit Selectable NotSelected i
