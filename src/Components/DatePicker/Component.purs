module Ocelot.Component.DatePicker where

import Prelude

import Data.Array (index, length, mapWithIndex)
import Data.Date (Date, Month, Year, canonicalDate, month, year)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (trim)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.Layout as Layout
import Ocelot.Component.DatePicker.Utils as Utils
import Ocelot.Data.DateTime as ODT
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Setters as Setters

----------
-- Calendar Items

data CalendarItem
  = CalendarItem SelectableStatus SelectedStatus BoundaryStatus Date

data SelectableStatus
  = NotSelectable
  | Selectable

data SelectedStatus
  = NotSelected
  | Selected

data BoundaryStatus
  = OutOfBounds
  | InBounds



type AddedState =
  ( targetDate :: Tuple Year Month
  , calendarItems :: Array CalendarItem
  , selection :: Maybe Date
  )
type Input =
  { targetDate :: Maybe (Tuple Year Month)
  , selection :: Maybe Date
  }

data Direction
  = Prev
  | Next

data Action
  = Initialize
  | ToggleYear  Direction
  | ToggleMonth Direction
  | Synchronize

data Query a
  = GetSelection (Maybe Date -> a)
  | SetSelection (Maybe Date) a

data Message
  = SelectionChanged (Maybe Date)
  | VisibilityChanged Select.Visibility
  | Searched String

type ChildSlots = ()
type SelfSlot index = Select.Slot Query ChildSlots Message index

dropdownClasses :: Array HH.ClassName
dropdownClasses = HH.ClassName <$>
  [ "pin-t"
  , "pin-l"
  , "p-6"
  , "bg-white"
  , "text-center"
  , "text-lg"
  ]

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML (Select.Query Query ChildSlots) Input Message m
component = Select.component mkInput $ Select.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleQuery = handleQuery
  , handleEvent = handleEvent
  , initialize = Just Initialize
  }
  where
    mkInput :: Input -> Select.Input AddedState
    mkInput { targetDate, selection } =
      let
        targetDate' = fromMaybe (Tuple (ODT.unsafeMkYear 2001) (ODT.unsafeMkMonth 1)) targetDate
      in
        { inputType: Select.Text
        , search: Nothing
        , debounceTime: Nothing
        , getItemCount: \state -> length state.calendarItems

        -- labels from AddedState
        , calendarItems: generateCalendarRows selection (fst targetDate') (snd targetDate')
        , targetDate: targetDate'
        , selection
        }

    handleAction :: Action -> H.HalogenM (Select.State AddedState) (Select.Action Action) ChildSlots Message m Unit
    handleAction = case _ of
      Initialize -> do
        { selection } <- H.get
        d <- H.liftEffect nowDate
        let d' = fromMaybe d selection
        H.modify_ _ { targetDate = Tuple (year d') (month d') }
        handleAction Synchronize

      ToggleMonth dir -> do
        st <- H.get
        let y = fst st.targetDate
            m = snd st.targetDate
            newDate = case dir of
               Next -> ODT.nextMonth (canonicalDate y m bottom)
               Prev -> ODT.prevMonth (canonicalDate y m bottom)
        H.modify_ _ { targetDate = Tuple (year newDate) (month newDate) }
        handleAction Synchronize

      -- We ought to be able to navigate years in the date picker
      ToggleYear dir -> do
        st <- H.get
        let y = fst st.targetDate
            m = snd st.targetDate
            newDate = case dir of
               Next -> ODT.nextYear (canonicalDate y m bottom)
               Prev -> ODT.prevYear (canonicalDate y m bottom)
        H.modify_ _ { targetDate = Tuple (year newDate) (month newDate) }
        handleAction Synchronize

      Synchronize -> do
        { targetDate: Tuple y m, selection } <- H.get
        let calendarItems = generateCalendarRows selection y m
        let update = case selection of
              Just date -> _ { search = ODT.formatDate date }
              otherwise -> identity
        H.modify_ (update <<< _ { calendarItems = calendarItems })

    handleQuery :: forall a. Query a -> H.HalogenM (Select.State AddedState) (Select.Action Action) ChildSlots Message m (Maybe a)
    handleQuery = case _ of
      GetSelection reply -> do
        { selection } <- H.get
        pure (Just (reply selection))

      SetSelection selection a -> do
        st <- H.get
        let targetDate = maybe st.targetDate (\d -> Tuple (year d) (month d)) selection
        H.modify_ _ { selection = selection, targetDate = targetDate }
        handleAction Synchronize
        pure (Just a)

    handleEvent :: Select.Event -> H.HalogenM (Select.State AddedState) (Select.Action Action) ChildSlots Message m Unit
    handleEvent = case _ of
      Select.Selected idx -> do
        calendarItems <- H.gets _.calendarItems
        case index calendarItems idx of
          Nothing -> do
            H.raise $ SelectionChanged Nothing
          Just (CalendarItem _ _ _ date) -> do
            H.raise $ SelectionChanged (Just date)

      Select.VisibilityChanged visibility -> do
        H.raise $ VisibilityChanged visibility

      Select.Searched str -> do
        -- Note: the below code is unnecessary as Select does this for us now
        --
        -- H.modify_ _ { search = text }
        -- we don't actually want to match on search, we want to wait
        -- until they hit ENTER and then we'll try to match their search
        -- pure a

        pure unit

    render :: Select.State AddedState -> H.ComponentHTML (Select.Action Action) ChildSlots m
    render st =
      HH.div_
        [ renderSearch
        , renderSelect targetYear targetMonth st
        ]
      where
        targetYear  = fst st.targetDate
        targetMonth = snd st.targetDate

        -- The page element that will hold focus, capture key events, etcetera
        renderSearch =
          Input.input
            ( Setters.setInputProps [ HP.value st.search ] )

        renderSelect y m cst =
          HH.div
            [ css "relative" ]
            $ if cst.visibility == Select.On
              then [ renderCalendar y m cst ]
              else [ ]

-- The overall container for the calendar
renderCalendar
  :: forall m
   . MonadAff m
  => Year
  -> Month
  -> Select.State AddedState
  -> H.ComponentHTML (Select.Action Action) ChildSlots m
renderCalendar y m cst =
  Layout.popover
    ( Setters.setContainerProps
      [ HP.classes dropdownClasses ]
    )
    [ calendarNav
    , calendarHeader
    , HH.div_ $ renderRows $ Utils.rowsFromArray cst.calendarItems
    ]
  where
    -- A helper function that will turn a date into a year/month date string
    fmtMonthYear = either (const "-") identity
      <<< formatDateTime "MMMM YYYY"
      <<< toDateTime
      <<< fromDate

    -- We generally will use this value: the current month and year
    monthYear = fmtMonthYear $ canonicalDate y m bottom

    -- Given a string ("Month YYYY"), creates the calendar navigation.
    -- Could be much better in rendering
    calendarNav :: H.ComponentHTML (Select.Action Action) ChildSlots m
    calendarNav =
      Format.contentHeading
        [ css "flex" ]
        [ arrowButton (\_ -> Just (Select.Action (ToggleMonth Prev))) [ Icon.chevronLeft_ ]
        , dateHeader
        , arrowButton (\_ -> Just (Select.Action (ToggleMonth Next))) [ Icon.chevronRight_ ]
        ]
      where
        -- The below comment is likely now outdated, now that we're using
        -- a higher version of Select:
        --
        -- We need to embed functionality into these arrow buttons so they trigger
        -- queries in the parent. Let's do that here. To make this work, remember
        -- you're writing in `Select`'s HTML type and you have to wrap your constructors
        -- in Raise.
        arrowButton onClickHandler =
          Button.buttonClear
            -- HE.onClick $ Select.always $ Select.raise $ H.action q
            [ HE.onClick onClickHandler
            , css "text-grey-70 p-3"
            ]

        -- Show the month and year
        dateHeader =
          HH.div
            [ css "flex-1" ]
            [ HH.text monthYear ]

    calendarHeader =
      HH.div
        [ css "flex text-grey-70" ]
        ( headers <#>
          \day ->
            HH.div
              [ css "w-14 h-14 flex items-center justify-center" ]
              [ HH.text day ]
        )
      where
        headers = [ "S", "M", "T", "W", "T", "F", "S" ]

    -- Here we'll render out our dates as rows in the calendar.
    renderRows =
      mapWithIndex (\row subArr -> renderRow (row * 7) subArr)
      where
        renderRow offset items =
          HH.div
            [ css "flex font-light" ]
            ( mapWithIndex
              (\column item -> renderItem (column + offset) item) items
            )

    renderItem index item =
      HH.div
      -- Here's the place to use info from the item to render it in different
      -- states.
      -- if highlightedIndex == Just index then 'highlight' else 'dont'
      -- Because there are so many possible states, what about a helper like
      -- getCalendarStyles?
        ( maybeSetItemProps index item
          [ css
            $ trim
            $ "w-14 h-14 rounded-full relative "
              <> "flex items-center justify-center "
              <> "transition-1/4 border border-white "
              <> "before:no-content before:transition-1/4 "
              <> "before:w-full before:h-full "
              <> "before:absolute before:pin-t before:pin-l "
              <> (getCalendarStyles item)
          ]
        )
        -- printDay will format our item correctly
        [ HH.text $ printDay item ]
      where
        -- If the calendar item is selectable,
        -- then augment the props with the correct click events.
        -- if not, then just don't provide the props at all.
        -- this is an easy way to "disable" functionality in the calendar.
        maybeSetItemProps i (CalendarItem Selectable _ _ _) props =
          Setters.setItemProps i props
        maybeSetItemProps _ _ props = props

        -- Get the correct styles for a calendar item, dependent on its statuses
        getCalendarStyles :: CalendarItem -> String
        getCalendarStyles i
          = trim $ getSelectableStyles i
          <> " " <> getSelectedStyles i
          <> " " <> getBoundaryStyles i
          where
            getSelectableStyles :: CalendarItem -> String
            getSelectableStyles (CalendarItem NotSelectable _ _ _) =
              mempty
            getSelectableStyles _ =
              "cursor-pointer hover:border hover:border-blue-88"

            getSelectedStyles :: CalendarItem -> String
            getSelectedStyles (CalendarItem _ Selected _ _) =
              "bg-blue-88 text-white before:scale-1"
            getSelectedStyles _ =
              "before:scale-0"

            getBoundaryStyles :: CalendarItem -> String
            getBoundaryStyles (CalendarItem _ _ OutOfBounds _) =
              "text-grey-90"
            getBoundaryStyles _ = mempty

        -- Just a simple helper to format our CalendarItem into a day
        -- we can print out
        printDay :: CalendarItem -> String
        printDay (CalendarItem _ _ _ d) = printDay' d
          where
            printDay' :: Date -> String
            printDay' = (either (const "-") identity)
              <<< formatDateTime "D"
              <<< toDateTime
              <<< fromDate


----------
-- Other helpers for the file

-- Generate a standard set of dates from a year and month.
generateCalendarRows
  :: Maybe Date
  -> Year
  -> Month
  -> Array CalendarItem
generateCalendarRows selection y m = lastMonth <> thisMonth <> nextMonth
  where
    { pre, body, post, all } = Utils.alignByWeek y m
    outOfBounds = map (generateCalendarItem selection OutOfBounds)
    lastMonth   = outOfBounds pre
    nextMonth   = outOfBounds post

    thisMonth = body <#> (generateCalendarItem selection InBounds)

generateCalendarItem
  :: Maybe Date
  -> BoundaryStatus
  -> Date
  -> CalendarItem
generateCalendarItem Nothing bound i =
  CalendarItem Selectable NotSelected bound i
generateCalendarItem (Just d) bound i
  | d == i = CalendarItem Selectable Selected bound i
  | otherwise = CalendarItem Selectable NotSelected bound i
