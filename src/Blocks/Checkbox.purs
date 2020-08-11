module Ocelot.Block.Checkbox where

import Prelude

import DOM.HTML.Indexed (HTMLinput, HTMLlabel)
import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))
import TailwindClasses as TailwindClasses

labelClasses :: Array HH.ClassName
labelClasses =
  [ TailwindClasses.flex
  , HH.ClassName "flex-row"
  , HH.ClassName "inline-block"
  , HH.ClassName "py-2"
  , HH.ClassName "cursor-pointer"
  , HH.ClassName "text-black-20"
  , HH.ClassName "items-center"
  , HH.ClassName "text-left" -- styles get messed up otherwise
  ]

inputClasses :: Array HH.ClassName
inputClasses =
  -- start shared custom classes defined for radios --
  [ HH.ClassName "!disabled:sibling:bg-white"
  , HH.ClassName "disabled:sibling:bg-grey-95"
  , HH.ClassName "checked:sibling:before:opacity-100"
  , HH.ClassName "checked:sibling:before:scale-1"
  , HH.ClassName "checked:!disabled:sibling:border-blue-88"
  , HH.ClassName "focus:sibling:border-blue-88"
  , HH.ClassName "!checked:sibling:before:opacity-0"
  , HH.ClassName "!checked:sibling:before:scale-0"
  , HH.ClassName "!focus:hover:!checked:!disabled:sibling:border-grey-70"
  , HH.ClassName "focus:sibling:shadow"
  , HH.ClassName "checked:!disabled:sibling:before:bg-blue-88"
  , HH.ClassName "checked:disabled:sibling:before:bg-grey-80"
  , HH.ClassName "checked:disabled:sibling:border-grey-80"
  -- end shared custom radio classes --
  , HH.ClassName "checked:sibling:after:opacity-100"
  , HH.ClassName "checked:sibling:after:scale-1"
  , HH.ClassName "!checked:sibling:after:opacity-0"
  , HH.ClassName "!checked:sibling:after:scale-0"
  ]

checkboxClasses :: Array HH.ClassName
checkboxClasses =
  [ TailwindClasses.relative
  , HH.ClassName "content-box"
  , HH.ClassName "border-2"
  , HH.ClassName "border-solid"
  , HH.ClassName "h-5"
  , HH.ClassName "w-5"
  , HH.ClassName "flex-none"
  , HH.ClassName "no-content"
  , HH.ClassName "mr-3"
  , TailwindClasses.rounded
  , HH.ClassName "before:transition-1/4-bounce"
  , HH.ClassName "before:absolute"
  , HH.ClassName "before:h-full"
  , HH.ClassName "before:w-full"
  , HH.ClassName "before:no-content"
  , HH.ClassName "after:transition-1/4-bounce"
  , HH.ClassName "after:absolute"
  , HH.ClassName "after:w-full"
  , HH.ClassName "after:h-2"
  , HH.ClassName "after:border-l-2"
  , HH.ClassName "after:border-b-2"
  , HH.ClassName "after:border-white"
  , HH.ClassName "after:no-content"
  , HH.ClassName "after:rotate-315"
  , HH.ClassName "after:shadow"
  ]

checkbox
  :: ∀ p i
   . Array (HH.IProp HTMLlabel i)
  -> Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
checkbox iprops inprops html =
  HH.label
    ( [ HP.classes labelClasses ] <&> iprops )
    ( [ HH.input
        ( [ HP.classes inputClasses
          , HP.type_ InputCheckbox
          ] <&> inprops
        )
      , HH.span [ HP.classes checkboxClasses ] []
      ]
      <> html
    )

checkbox_
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
checkbox_ = checkbox []
