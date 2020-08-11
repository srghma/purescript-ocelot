module Ocelot.Block.Toggle (toggle) where

import Prelude

import TailwindClasses as TailwindClasses
import DOM.HTML.Indexed (HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(InputCheckbox))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type ToggleProps =
  { label :: String }

labelClasses :: Array HH.ClassName
labelClasses =
  [ TailwindClasses.flex
  , HH.ClassName "flex-row"
  , HH.ClassName "items-center"
  , HH.ClassName "inline-block"
  , HH.ClassName "py-1"
  , HH.ClassName "cursor-pointer"
  , HH.ClassName "leading-loose"
  , HH.ClassName "text-black-20"
  ]

inputClasses :: Array HH.ClassName
inputClasses =
  [ HH.ClassName "checked:sibling:bg-blue-88"
  , HH.ClassName "checked:sibling:pl-5"
  , HH.ClassName "!checked:sibling:bg-grey-80"
  , HH.ClassName "!checked:sibling:pr-5"
  , HH.ClassName "offscreen"
  ]

toggleClasses :: Array HH.ClassName
toggleClasses =
  [ HH.ClassName "transition-1/8"
  , HH.ClassName "inline-flex"
  , HH.ClassName "justify-center"
  , HH.ClassName "items-center"
  , HH.ClassName "content-box"
  , HH.ClassName "h-5"
  , HH.ClassName "w-5"
  , HH.ClassName "p-1"
  , HH.ClassName "rounded-full"
  , HH.ClassName "mr-3"
  , HH.ClassName "before:bg-white"
  , HH.ClassName "before:h-full"
  , HH.ClassName "before:w-full"
  , HH.ClassName "before:rounded-full"
  , HH.ClassName "before:no-content"
  , HH.ClassName "before:shadow"
  ]

toggle
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
toggle iprops =
  HH.label
    [ HP.classes labelClasses ]
    [ HH.input iprops'
    , HH.span [ HP.classes toggleClasses ] []
    ]
    where
      iprops' = iprops <>
        [ HP.classes inputClasses
        , HP.type_ InputCheckbox
        ]
