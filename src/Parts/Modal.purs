-- | Not an entire component, but pieces that only make sense within
-- | the context of components that meet the constraints on these
-- | functions.
module Ocelot.Part.Modal where

import Prelude

import TailwindClasses as TailwindClasses
import DOM.HTML.Indexed (HTMLdiv)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css, (<&>))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

----------
-- Eval Partials

-- A function that will register an event source on the window
initializeWith
  :: ∀ state action slots output m
   . MonadAff m
  => (KE.KeyboardEvent -> Maybe action)
  -> H.HalogenM state action slots output m H.SubscriptionId
initializeWith toAction = do
  document <- H.liftEffect $ document =<< window
  H.subscribe
    $ ES.eventListenerEventSource
      KET.keydown
      (HTMLDocument.toEventTarget document)
      (toAction <=< KE.fromEvent)

whenClose
  :: ∀ state action slots output m
   . MonadAff m
  => KE.KeyboardEvent
  -> H.SubscriptionId
  -> H.HalogenM state action slots output m Unit
  -> H.HalogenM state action slots output m Unit
whenClose ev sid close =
  when (KE.code ev == "Escape") do
    H.unsubscribe sid
    close

----------
-- Render Partials

-- Modals already come with the ClickOutside event baked in, while
-- end users are responsible for handling it somehow.
modal
  :: ∀ action slots m
   . action
  -> Array (HH.IProp HTMLdiv action)
  -> Array (H.ComponentHTML action slots m)
  -> H.ComponentHTML action slots m
modal click iprops html =
  HH.div_
    [ HH.div
        [ HP.classes backgroundClasses
        , HE.onClick $ Just <<< const click
        ]
        []
    , HH.div
        ( [ HP.classes modalClasses ] <&> iprops )
        html
    ]

modal_
  :: ∀ action slots m
   . action
  -> Array (H.ComponentHTML action slots m)
  -> H.ComponentHTML action slots m
modal_ query = modal query []


-----------
-- Blocks

type HeaderProps p i =
  { buttons :: Array (HH.HTML p i)
  , title :: Array (HH.HTML p i)
  }

backgroundClasses :: Array HH.ClassName
backgroundClasses =
  [ HH.ClassName "fixed"
  , HH.ClassName "inset-0"
  , HH.ClassName "bg-black-modal-a90"
  , HH.ClassName "fade-in"
  , HH.ClassName "z-10"
  ]

modalClasses :: Array HH.ClassName
modalClasses =
  [ HH.ClassName "fixed"
  , HH.ClassName "inset-x-0"
  , HH.ClassName "top-0"
  , HH.ClassName "my-20"
  , HH.ClassName "m-auto"
  , HH.ClassName "max-w-lg"
  , HH.ClassName "slide-down"
  , HH.ClassName "z-10"
  ]

bodyClasses :: Array HH.ClassName
bodyClasses =
  [ HH.ClassName "relative"
  , HH.ClassName "bg-grey-95"
  , HH.ClassName "overflow-auto"
  , HH.ClassName "max-h-full"
  , HH.ClassName "w-full"
  , HH.ClassName "flex-col"
  , TailwindClasses.flex
  , HH.ClassName "rounded-b"
  ]

body
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
body iprops html =
  HH.div
    ( [ HP.classes bodyClasses ] <&> iprops )
    html

body_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
body_ = body []

headerClasses :: Array HH.ClassName
headerClasses =
  [ HH.ClassName "h-24"
  , TailwindClasses.flex
  ]

outerHeaderClasses :: Array HH.ClassName
outerHeaderClasses =
  [ HH.ClassName "bg-white"
  , HH.ClassName "w-full"
  , HH.ClassName "px-6"
  , HH.ClassName "items-center"
  , TailwindClasses.flex
  , HH.ClassName "rounded-t"
  ]

innerHeaderClasses :: Array HH.ClassName
innerHeaderClasses =
  [ HH.ClassName "w-full"
  , HH.ClassName "items-center"
  , HH.ClassName "mx-auto"
  , TailwindClasses.flex
  ]

header
  :: ∀ p i
   . HeaderProps p i
  -> HH.HTML p i
header props =
  HH.div
    [ HP.classes headerClasses ]
    [ HH.header
      [ HP.classes outerHeaderClasses ]
      ( [ HH.div
        [ HP.classes innerHeaderClasses ]
          [ Format.subHeading
            [ css "mb-0" ]
            props.title
          ]
        ]
        <> props.buttons
      )
    ]
