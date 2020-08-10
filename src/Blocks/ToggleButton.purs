module Ocelot.Block.ToggleButton where

import Prelude

import DOM.HTML.Indexed (HTMLinput, HTMLspan)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

toggleButton
  :: ∀ p i
   . Array (HH.IProp HTMLspan i)
  -> Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
toggleButton iprops inprops html =
  HH.label_
  [ HH.input inputProps
  , HH.span labelProps html
  ]
  where
  inputProps =
    [ HP.classes inputClasses
    , HP.type_ HP.InputRadio
    ] <&> inprops

  labelProps =
    [ HP.classes toggleButtonClasses ] <&> iprops

inputClasses :: Array HH.ClassName
inputClasses =
  [ HH.ClassName "checked:neighbor:bg-grey-50"
  , HH.ClassName "checked:neighbor:text-white"
  , HH.ClassName "checked:neighbor:border-grey-50"
  , HH.ClassName "!checked:neighbor:hover:bg-grey-80"
  , HH.ClassName "!checked:neighbor:hover:text-black-10!"
  , HH.ClassName "offscreen"
  ]

toggleButtonClasses :: Array HH.ClassName
toggleButtonClasses =
  [ HH.ClassName "no-outline"
  , HH.ClassName "px-4"
  , HH.ClassName "py-2"
  , HH.ClassName "disabled:opacity-50"
  , HH.ClassName "disabled:cursor-default"
  , HH.ClassName "!disabled:cursor-pointer"
  , HH.ClassName "bg-white"
  , HH.ClassName "border-grey-80"
  , HH.ClassName "border-2"
  , HH.ClassName "focus:bg-grey-50-a30"
  , HH.ClassName "text-black-20"
  , HH.ClassName "inline-block"
  ]
