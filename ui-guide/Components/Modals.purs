module UIGuide.Components.Modals where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Modal as Modal
import Ocelot.Components.Typeahead as TACore
import Ocelot.Components.Typeahead.Input as TA
import Ocelot.HTML.Properties (css)
import UIGuide.Components.Typeaheads (ChildSlots, _location, _user)
import UIGuide.Utilities.Async as Async

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML Query Input Message m
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
    eval :: Query ~> H.HalogenM State Query (ChildSlots m) Message m
    eval = case _ of
      NoOp a -> pure a

    render :: State -> H.ComponentHTML Query (ChildSlots m) m
    render _ =
      Modal.modal_
        [ Modal.header
          { buttons:
              [ HH.a
                [ HP.classes ( Format.linkDarkClasses <> [ HH.ClassName "mr-4" ] ) ]
                [ HH.text "Cancel" ]
              , Button.buttonPrimary_ [ HH.text "Submit" ]
              ]
          , title: [ HH.text "Editing" ]
          }
        , Modal.body_
          [ Card.card
            [ css "flex-1 m-10" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "locations"
              }
              [ HH.slot _location 0 TACore.component
                (TA.defAsyncMulti
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "locations"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( const Nothing )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "locations"
              }
              [ HH.slot _user 0 TACore.component
                (TA.defAsyncMulti
                  [ HP.placeholder "Search users..."
                  , HP.id_ "users"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( const Nothing )
              ]
            ]
          ]
      ]
