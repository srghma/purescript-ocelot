module UIGuide.Components.Dropdown where

import Prelude

import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Format (caption_) as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Blocks.Choice as Choice
import Ocelot.Components.Dropdown as DD
import Ocelot.Components.Dropdown.Render as DR
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Setters as SelectSetters
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a
  = HandleDropdown (DD.Message Query String) a
  | HandleChoice (Select.Message Query Platform) a

type Input = Unit

type Message = Void

type ChildSlots m =
  ( dropdown :: DD.Slot Query String m Unit
  , select :: Select.Slot Query () Platform m Unit
  )

_dropdown = SProxy :: SProxy "dropdown"
_select = SProxy :: SProxy "select"

data Platform
  = Facebook
  | Twitter

component
  :: ∀ m
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
    eval
      :: Query
      ~> H.HalogenM State Query (ChildSlots m) Message m
    eval = case _ of
      HandleDropdown message a -> case message of
        DD.Selected x -> do
          H.liftEffect (log x)
          H.modify_ identity
          pure a

        _ -> pure a

      HandleChoice message a -> case message of
        Select.Selected x -> a <$ do
          H.liftEffect $ case x of
            Facebook -> log "Facebook"
            Twitter -> log "Twitter"
          H.query _select unit ( Select.setVisibility Select.Off )

        _ -> pure a

    render
      :: State
      -> H.ComponentHTML Query (ChildSlots m) m
    render state =
      HH.div_
        [ Documentation.block_
          { header: "Dropdown"
          , subheader: "A dropdown list of selectable items."
          }
          [ Backdrop.backdrop_
            [ Backdrop.content_
              [ HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Standard" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Nothing
                  , items
                  , render: renderDropdown Button.button
                  }
                  ( HE.input HandleDropdown )
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Just "Kilchoman Blue Label"
                  , items
                  , render: renderDisabledDropdown Button.button
                  }
                  ( HE.input HandleDropdown )
                ]
              ]
            ]
          , Backdrop.backdropWhite_
            [ Backdrop.content_
              [ HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Primary" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Nothing
                  , items
                  , render: renderDropdown Button.buttonPrimary
                  }
                  ( HE.input HandleDropdown )
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Just "Kilchoman Blue Label"
                  , items
                  , render: renderDisabledDropdown Button.buttonPrimary
                  }
                  ( HE.input HandleDropdown )
                ]
              ]
            ]
          , Backdrop.backdropDark_
            [ Backdrop.content_
              [ HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Dark" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Nothing
                  , items
                  , render: renderDropdown Button.buttonDark
                  }
                  ( HE.input HandleDropdown )
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Just "Kilchoman Blue Label"
                  , items
                  , render: renderDisabledDropdown Button.buttonDark
                  }
                  ( HE.input HandleDropdown )
                ]
              ]
            ]
          ]
        , Documentation.block_
          { header: "Choice"
          , subheader: "A specialized dropdown for making selections."
          }
          [ Backdrop.backdrop
            [ css "h-40 flex items-center justify-center" ]
            [ HH.slot
                _select
                unit
                Select.component
                selectInput
                ( HE.input HandleChoice )
            ]
          ]
        ]

      where
        items :: Array String
        items =
          [ "Lagavulin 16"
          , "Kilchoman Blue Label"
          , "Laphroaig"
          , "Ardbeg"
          ]

        renderDropdown
          :: (∀ p i. DR.ButtonFn p i)
          -> DD.State String
          -> H.ComponentHTML (DD.Query Query String m) (DD.ChildSlots Query String m) m
        renderDropdown btnFn = DR.render $ DR.defDropdown btnFn [ ] identity "Pick One"

        renderDisabledDropdown
          :: (∀ p i. DR.ButtonFn p i)
          -> DD.State String
          -> H.ComponentHTML (DD.Query Query String m) (DD.ChildSlots Query String m) m
        renderDisabledDropdown btnFn =
          DR.render $ DR.defDropdown btnFn [ HP.disabled true ] identity "Pick One"

        selectInput :: Select.Input Query () Platform m
        selectInput =
          { debounceTime: Nothing
          , initialSearch: Nothing
          , inputType: Select.Toggle
          , items: [ Facebook, Twitter ]
          , render: renderPlatformChoice
          }

        renderPlatformChoice state' =
          HH.div
            [ css "flex items-center flex-col" ]
            [ menu
            , Button.buttonPrimary
              ( SelectSetters.setToggleProps [] )
              [ HH.text "Create Campaign Group" ]
            ]
            where
              visibilityClasses = case state'.visibility of
                Select.On -> css ""
                Select.Off -> css "hidden"

              menu =
                Choice.choice
                  ( SelectSetters.setContainerProps [ visibilityClasses ] )
                  [ Choice.header_
                    [ HH.span
                      [ css "font-medium text-grey-50" ]
                      [ HH.text "Advertise on..." ]
                    ]
                  , Choice.body_ $
                      mapWithIndex
                        ( \index item ->
                            Choice.option
                              ( SelectSetters.setItemProps
                                  index
                                  [ if Just index == state'.highlightedIndex
                                      then HP.classes Choice.highlightedOptionClasses
                                      else HP.classes []
                                  ]
                              )
                              ( renderPlatform item )
                        )
                        state'.items
                  ]

              renderPlatform = case _ of
                Facebook ->
                  [ HH.div_
                    [ Icon.facebook
                      [ css "text-fb-blue text-4xl" ]
                    ]
                  , HH.div_
                    [ HH.p
                      [ css "text-black-20 font-light" ]
                      [ HH.text "Facebook" ]
                    ]
                  ]
                Twitter ->
                  [ HH.div_
                    [ Icon.twitter
                      [ css "text-tw-blue text-4xl" ] ]
                  , HH.div_
                    [ HH.p
                      [ css "text-black-20 font-light" ]
                      [ HH.text "Twitter" ]
                    ]
                  ]

