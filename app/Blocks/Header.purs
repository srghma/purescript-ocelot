module UIGuide.Blocks.Header (header) where

import Prelude
import TailwindClasses as TailwindClasses
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

headerClasses :: Array HH.ClassName
headerClasses =
  [ TailwindClasses.bg_blue_900
  , TailwindClasses.h_12
  , TailwindClasses.w_full
  ]

header :: ∀ p i. HH.HTML p i
header =
  HH.header
    [ HP.classes headerClasses ]
    []
