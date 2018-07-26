module UIGuide.Components.Typeaheads where

import Prelude

import Data.Array (head, take)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Components.Typeahead as TA
import Ocelot.Components.Typeahead.Input as TA.Input
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import UIGuide.Utilities.Async as Async

----------
-- Component Types

type State = Unit

data Query a
  = NoOp a
  | HandleTypeaheadUser Int (TA.Message Query Async.User) a
  | HandleTypeaheadLocation Int (TA.Message Query Async.Location) a
  | Initialize a

----------
-- Child paths

type ChildSlots m =
  ( location :: TA.Slot Query () Async.Location Async.Err m Int
  , user :: TA.Slot Query () Async.User Async.Err m Int
  )

_location = SProxy :: SProxy "location"
_user = SProxy :: SProxy "user"

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
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }
  where
    -- For the sake of testing and visual demonstration, we'll just render
    -- out a bunch of selection variants in respective slots
    render
      :: State
      -> H.ComponentHTML Query (ChildSlots m) m
    render _ = cnDocumentationBlocks

    eval
      :: Query
      ~> H.HalogenM State Query (ChildSlots m) Void m
    eval (NoOp next) = pure next


    -- No longer necessary to fetch data. Treat it just like a Sync typeahead.
    eval (HandleTypeaheadUser slot m next) = pure next
    eval (HandleTypeaheadLocation slot m next) = pure next

    eval (Initialize next) = do
      _ <- H.queryAll _location $ H.action $ TA.ReplaceItems Loading
      _ <- H.queryAll _user $ H.action $ TA.ReplaceItems Loading
      remoteLocations <- H.liftAff $ Async.loadFromSource Async.locations ""
      _ <- case remoteLocations of
        items@(Success _) -> do
          _ <- H.queryAll _location $ H.action $ TA.ReplaceItems items
          pure unit
        otherwise -> pure unit
      remoteUsers <- H.liftAff $ Async.loadFromSource Async.users ""
      _ <- case remoteUsers of
        items@(Success _) -> do
          _ <- H.queryAll _user $ H.action $ TA.ReplaceItems items
          pure unit
        otherwise -> pure unit
      selectedLocations <- H.liftAff $ Async.loadFromSource Async.locations "an"
      _ <- case selectedLocations of
        Success xs -> do
          _ <- H.query _location 1
            $ H.action
            $ TA.ReplaceSelections
            $ TA.One
            $ head xs
          _ <- H.query _location 3
            $ H.action
            $ TA.ReplaceSelections
            $ TA.Many
            $ take 4 xs
          _ <- H.query _location 5
            $ H.action
            $ TA.ReplaceSelections
            $ TA.One
            $ head xs
          pure unit
        otherwise -> pure unit
      selectedUsers <- H.liftAff $ Async.loadFromSource Async.users "an"
      _ <- case selectedUsers of
        Success xs -> do
          _ <- H.query _user 1
            $ H.action
            $ TA.ReplaceSelections
            $ TA.One
            $ head xs
          _ <- H.query _user 3
            $ H.action
            $ TA.ReplaceSelections
            $ TA.Many
            $ take 4 xs
          _ <- H.query _user 5
            $ H.action
            $ TA.ReplaceSelections
            $ TA.Many
            $ take 4 xs
          pure next
        otherwise -> pure next
      _ <- H.query _location 6 $ H.action $ TA.ReplaceItems $ Failure ""
      _ <- H.query _user 6 $ H.action $ TA.ReplaceItems $ Failure ""
      _ <- H.query _location 7 $ H.action $ TA.ReplaceItems Loading
      _ <- H.query _user 7 $ H.action $ TA.ReplaceItems Loading
      pure next

----------
-- HTML

content :: ∀ p i. Array (HH.HTML p (i Unit)) -> HH.HTML p (i Unit)
content = Backdrop.content [ css "flex" ]

cnDocumentationBlocks :: ∀ m
  . MonadAff m
 => H.ComponentHTML Query (ChildSlots m) m
cnDocumentationBlocks =
  HH.div_
    [ Documentation.block_
      { header: "Typeaheads - Single-Select"
      , subheader: "Uses string input to search predetermined entries. User selects one of these entries."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your favorite destination."
              , error: Nothing
              , inputId: "location"
              }
              [ HH.slot _location 0 TA.component
                (TA.Input.defAsyncSingle
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "location"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 0 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard Hydrated" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your favorite destination."
              , error: Nothing
              , inputId: "location-hydrated"
              }
              [ HH.slot _location 1 TA.component
                (TA.Input.defAsyncSingle
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "location-hydrated"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 1 )
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your favorite companion."
              , error: Nothing
              , inputId: "user"
              }
              [ HH.slot _user 0 TA.component
                (TA.Input.defAsyncSingle
                  [ HP.placeholder "Search users..."
                  , HP.id_ "user"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 0 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render Hydrated" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your favorite companion."
              , error: Nothing
              , inputId: "user-hydrated"
              }
              [ HH.slot _user 1 TA.component
                (TA.Input.defAsyncSingle
                  [ HP.placeholder "Search users..."
                  , HP.id_ "user-hydrated"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 1 )
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "Typeaheads - Multi-Select"
      , subheader: "Uses string input to search predetermined entries. User selects one or more of these entries"
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Standard" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "locations"
              }
              [ HH.slot _location 2 TA.component
                (TA.Input.defAsyncMulti
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "locations"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 2 )
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
              [ HH.slot _location 3 TA.component
                (TA.Input.defAsyncMulti
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "locations"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 3 )
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "users"
              }
              [ HH.slot _user 2 TA.component
                (TA.Input.defAsyncMulti
                  [ HP.placeholder "Search users..."
                  , HP.id_ "users"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 2 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Custom Render Hydrated" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "users-hydrated"
              }
              [ HH.slot _user 3 TA.component
                (TA.Input.defAsyncMulti
                  [ HP.placeholder "Search users..."
                  , HP.id_ "users-hydrated"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 3 )
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "Typeaheads - State Variants"
      , subheader: "Typeaheads can also be in a disabled, loading or error state."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Single Select - Empty" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "disabled-locations-empty"
              }
              [ HH.slot _location 4 TA.component
                (TA.Input.defAsyncSingle
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "disabled-locations-empty"
                  , HP.disabled true
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 4 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Single Select - Hydrated" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "disabled-locations-hydrated"
              }
              [ HH.slot _location 5 TA.component
                (TA.Input.defAsyncSingle
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "disabled-locations-hydrated"
                  , HP.disabled true
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 5 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Error Single Select" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "error-locations"
              }
              [ HH.slot _location 6 TA.component
                (TA.Input.defAsyncSingle
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "error-locations"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 6 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Loading Single Select" ]
            , FormField.field_
              { label: "Locations"
              , helpText: Just "Search your top destinations."
              , error: Nothing
              , inputId: "loading-locations"
              }
              [ HH.slot _location 7 TA.component
                (TA.Input.defAsyncSingle
                  [ HP.placeholder "Search locations..."
                  , HP.id_ "loading-locations"
                  ]
                  ( Async.loadFromSource Async.locations )
                  Async.renderItemLocation
                )
                ( HE.input $ HandleTypeaheadLocation 7 )
              ]
            ]
          ]
        , content
          [ Card.card
            [ HP.class_ $ HH.ClassName "flex-1" ]
            [ HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Multi Select - Empty" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "disabled-users-empty"
              }
              [ HH.slot _user 4 TA.component
                (TA.Input.defAsyncMulti
                  [ HP.placeholder "Search Users..."
                  , HP.id_ "disabled-users-empty"
                  , HP.disabled true
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 4 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Disabled Multi Select - Hydrated" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "disabled-users-hydrated"
              }
              [ HH.slot _user 5 TA.component
                (TA.Input.defAsyncMulti
                  [ HP.placeholder "Search Users..."
                  , HP.id_ "disabled-users-hydrated"
                  , HP.disabled true
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 5 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Error Multi Select" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "error-users"
              }
              [ HH.slot _user 6 TA.component
                (TA.Input.defAsyncMulti
                  [ HP.placeholder "Search users..."
                  , HP.id_ "error-users"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 6 )
              ]
            , HH.h3
              [ HP.classes Format.captionClasses ]
              [ HH.text "Loading Multi Select" ]
            , FormField.field_
              { label: "Users"
              , helpText: Just "Search your top companions."
              , error: Nothing
              , inputId: "loading-users"
              }
              [ HH.slot _user 7 TA.component
                (TA.Input.defAsyncSingle
                  [ HP.placeholder "Search users..."
                  , HP.id_ "loading-users"
                  ]
                  ( Async.loadFromSource Async.users )
                  Async.renderItemUser
                )
                ( HE.input $ HandleTypeaheadUser 7 )
              ]
            ]
          ]
        ]
      ]
    ]
