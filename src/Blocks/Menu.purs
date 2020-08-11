module Ocelot.Block.Menu where

import Prelude

import TailwindClasses as TailwindClasses
import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

bodyClasses :: Array HH.ClassName
bodyClasses =
  [ TailwindClasses.flex
  , HH.ClassName "flex-col"
  , HH.ClassName "w-90"
  ]

body
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
body iprops =
  HH.div
    ( [ HP.classes bodyClasses ] <&> iprops )

body_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
body_ =
  body []

optionClasses :: Array HH.ClassName
optionClasses =
  [ HH.ClassName "bg-white"
  , TailwindClasses.flex
  , HH.ClassName "items-center"
  , HH.ClassName "justify-center"
  , HH.ClassName "cursor-pointer"
  ]

option
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
option iprops =
  HH.div
    ( [ HP.classes optionClasses ] <&> iprops )

option_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
option_ =
  option []

highlightedOptionClasses :: Array HH.ClassName
highlightedOptionClasses =
  [ HH.ClassName "bg-grey-97"
  ]

