module UIGuide.Blocks.Header (header) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

headerClasses :: Array HH.ClassName
headerClasses =
  [ HH.ClassName "bg-blue-darkest"
  , HH.ClassName "h-12"
  , HH.ClassName "w-full"
  ]

header :: ∀ p i. HH.HTML p i
header =
  HH.header
    [ HP.classes headerClasses ]
    []
