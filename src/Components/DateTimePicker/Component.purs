module Ocelot.Component.DateTimePicker where

import Prelude

import Data.DateTime (Date, DateTime(..), Month, Time, Year, date, time)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Ocelot.Component.DatePicker as DP
import Ocelot.Component.TimePicker as TP
import Ocelot.HTML.Properties (css)
import Select as Select

type State =
  { date :: Maybe Date
  , time :: Maybe Time
  , targetDate :: Maybe (Tuple Year Month)
  }

type Input =
  { selection :: Maybe DateTime
  , targetDate :: Maybe (Tuple Year Month)
  }

data Action
  = HandleDate DP.Message
  | HandleTime TP.Message

data Query a
  = GetSelection (Maybe DateTime -> a)
  | SetSelection (Maybe DateTime) a
  | SendDateQuery (DP.Query Unit) a
  | SendTimeQuery (TP.Query Unit) a

data Message
  = SelectionChanged (Maybe DateTime)
  | DateMessage DP.Message
  | TimeMessage TP.Message

_dateSlot :: SProxy "dateSlot"
_dateSlot = SProxy

_timeSlot :: SProxy "timeSlot"
_timeSlot = SProxy

type ChildSlots =
  ( dateSlot :: DP.SelfSlot Unit
  , timeSlot :: TP.SelfSlot Unit
  )
type SelfSlot index = H.Slot Query Message index

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery
                                     }
    }
  where
    initialState :: Input -> State
    initialState { selection, targetDate } =
      { date: date <$> selection
      , time: time <$> selection
      , targetDate
      }

    handleAction :: Action
                 -> H.HalogenM State Action ChildSlots Message m Unit
    handleAction = case _ of
      HandleDate msg -> case msg of
        DP.SelectionChanged date' -> do
          time' <- H.gets _.time
          H.raise $ SelectionChanged (DateTime <$> date' <*> time')
          H.modify_ _ { date = date' }

        _ -> H.raise $ DateMessage msg

      HandleTime msg -> case msg of
        TP.SelectionChanged time' -> do
          date' <- H.gets _.date
          H.raise $ SelectionChanged (DateTime <$> date' <*> time')
          H.modify_ _ { time = time' }

        _ -> H.raise $ TimeMessage msg

    handleQuery :: forall a.
                   Query a
                -> H.HalogenM State Action ChildSlots Message m (Maybe a)
    handleQuery = case _ of
      GetSelection reply -> do
        { time, date } <- H.get
        pure $ Just $ reply (DateTime <$> date <*> time)

      SetSelection dateTime a -> do
        let date' = date <$> dateTime
            time' = time <$> dateTime
        void $ H.query _dateSlot unit $ Select.Query $ DP.SetSelection date' a
        void $ H.query _timeSlot unit $ Select.Query $ TP.SetSelection time' a
        H.modify_ _ { date = date', time = time' }
        pure (Just a)

      SendDateQuery q a -> do
        _ <- H.query _dateSlot unit $ Select.Query q
        pure (Just a)

      SendTimeQuery q a -> do
        _ <- H.query _timeSlot unit $ Select.Query q
        pure (Just a)

    render :: State -> H.ComponentHTML Action ChildSlots m
    render { date, time, targetDate } =
      HH.div
        [ css "flex" ]
        [ HH.div
          [ css "w-1/2 mr-2" ]
          [ HH.slot _dateSlot unit DP.component
            { targetDate
            , selection: date
            }
            (Just <<< HandleDate)
          ]
        , HH.div
          [ css "flex-1" ]
          [ HH.slot _timeSlot unit TP.component
            { selection: time }
            (Just <<< HandleTime)
          ]
        ]
