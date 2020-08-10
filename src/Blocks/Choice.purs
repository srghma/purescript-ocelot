module Ocelot.Block.Choice where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

choiceClasses :: Array HH.ClassName
choiceClasses =
  [ HH.ClassName "absolute"
  , HH.ClassName "bg-white"
  , HH.ClassName "rounded-lg"
  , HH.ClassName "border"
  , HH.ClassName "border-grey-90"
  , HH.ClassName "shadow"
  , HH.ClassName "overflow-hidden"
  ]

choice
  :: ∀ p i
   .  Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
choice iprops =
  HH.div
    ( [ HP.classes choiceClasses ] <&> iprops )

choice_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
choice_ =
  choice []

headerClasses :: Array HH.ClassName
headerClasses =
  [ HH.ClassName "flex"
  , HH.ClassName "h-10"
  , HH.ClassName "justify-center"
  , HH.ClassName "items-center"
  , HH.ClassName "border-b"
  , HH.ClassName "border-grey-90"
  ]

header
  :: ∀ p i
   .  Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
header iprops =
  HH.div
    ( [ HP.classes headerClasses ] <&> iprops )

header_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
header_ =
  header []

bodyClasses :: Array HH.ClassName
bodyClasses =  [ HH.ClassName "flex" ]

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
  , HH.ClassName "flex"
  , HH.ClassName "flex-col"
  , HH.ClassName "items-center"
  , HH.ClassName "h-30"
  , HH.ClassName "justify-center"
  , HH.ClassName "w-40"
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
  [ HH.ClassName "bg-grey-97" ]

