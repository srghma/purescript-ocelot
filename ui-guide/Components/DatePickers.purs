module UIGuide.Components.DatePickers where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Components.DatePicker as DatePicker
import Ocelot.Components.TimePicker as TimePicker
import Ocelot.Data.DateTime (unsafeMkDate, unsafeMkTime)
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

----------
-- Component Types

type State = Unit

data Query a
  = NoOp a

type ChildSlots =
  ( datePicker :: DatePicker.Slot Int
  , timePicker :: TimePicker.Slot Int
  )

_datePicker = SProxy :: SProxy "datePicker"
_timePicker = SProxy :: SProxy "timePicker"

----------
-- Component definition

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML Query Unit Void m
component =
  H.component
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
    render
      :: State
      -> H.ComponentHTML Query ChildSlots m
    render _ = cnDocumentationBlocks

    eval
      :: Query
      ~> H.HalogenM State Query ChildSlots Void m
    eval (NoOp next) = pure next


----------
-- HTML

content :: ∀ p i. Array (HH.HTML p (i Unit)) -> HH.HTML p (i Unit)
content = Backdrop.content [ css "flex" ]

cnDocumentationBlocks :: ∀ m
  . MonadAff m
 => H.ComponentHTML Query ChildSlots m
cnDocumentationBlocks =
  HH.div_
    [ Documentation.block_
      { header: "Date Pickers"
      , subheader: "It's a date picker. Deal with it."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Standard" ]
            , FormField.fieldMid_
              { label: "Start"
              , helpText: Just "Choose a start date."
              , error: Nothing
              , inputId: "start-date"
              }
              [ HH.slot _datePicker 0 DatePicker.component
                { targetDate: Nothing
                , selection: Nothing
                }
                (const Nothing)
              ]
            ]
          ]
        , content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Hydrated" ]
            , FormField.fieldMid_
              { label: "End"
              , helpText: Just "Choose an end date."
              , error: Nothing
              , inputId: "end-date"
              }
              [ HH.slot _datePicker 1 DatePicker.component
                { targetDate: Nothing
                , selection: Just $ unsafeMkDate 2019 1 1
                }
                (const Nothing)
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "Time Pickers"
      , subheader: "It's a time picker. Deal with it."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Standard" ]
            , FormField.fieldMid_
              { label: "Start"
              , helpText: Just "Choose a start time."
              , error: Nothing
              , inputId: "start-time"
              }
              [ HH.slot _timePicker 0 TimePicker.component
                { selection: Nothing
                }
                (const Nothing)
              ]
            ]
          ]
        , content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Hydrated" ]
            , FormField.fieldMid_
              { label: "End"
              , helpText: Just "Choose an end time."
              , error: Nothing
              , inputId: "end-time"
              }
              [ HH.slot _timePicker 1 TimePicker.component
                { selection: Just $ unsafeMkTime 0 0 0 0
                }
                (const Nothing)
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "DateTime Pickers"
      , subheader: "We've combined them. Deal with it."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Standard" ]
            , FormField.field_
              { label: "Start"
              , helpText: Just "Choose a start date and time."
              , error: Nothing
              , inputId: "start"
              }
              [ HH.div
                [ css "flex" ]
                [ HH.div
                  [ css "flex-2 mr-4" ]
                  [ HH.slot _datePicker 3 DatePicker.component
                    { targetDate: Nothing
                    , selection: Nothing
                    }
                    (const Nothing)
                  ]
                , HH.div
                  [ css "flex-1 mr-16" ]
                  [ HH.slot _timePicker 3 TimePicker.component
                    { selection: Nothing
                    }
                    (const Nothing)
                  ]
                ]
              ]
            ]
          ]
        , content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Hydrated" ]
            , FormField.field_
              { label: "End"
              , helpText: Just "Choose an end date and time."
              , error: Nothing
              , inputId: "end"
              }
              [ HH.div
                [ css "flex" ]
                [ HH.div
                  [ css "flex-2 mr-4" ]
                  [ HH.slot _datePicker 4 DatePicker.component
                    { targetDate: Nothing
                    , selection: Just $ unsafeMkDate 2019 1 1
                    }
                    (const Nothing)
                  ]
                , HH.div
                  [ css "flex-1 mr-16" ]
                  [ HH.slot _timePicker 3 TimePicker.component
                    { selection: Just $ unsafeMkTime 0 0 0 0
                    }
                    (const Nothing)
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
