module Ocelot.Block.Radio where

import Prelude

import TailwindClasses as TailwindClasses
import DOM.HTML.Indexed (HTMLinput, HTMLlabel)
import DOM.HTML.Indexed.InputType (InputType(InputRadio))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

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
  , HH.ClassName "offscreen"
  ]

radioClasses :: Array HH.ClassName
radioClasses =
  [ HH.ClassName "inline-flex"
  , HH.ClassName "justify-center"
  , HH.ClassName "items-center"
  , HH.ClassName "content-box"
  , HH.ClassName "border-2"
  , HH.ClassName "border-solid"
  , HH.ClassName "h-4"
  , HH.ClassName "w-4"
  , HH.ClassName "p-1"
  , HH.ClassName "flex-none"
  , HH.ClassName "no-content"
  , HH.ClassName "rounded-full"
  , HH.ClassName "mr-3"
  , HH.ClassName "before:transition-1/4-bounce"
  , HH.ClassName "before:h-full"
  , HH.ClassName "before:w-full"
  , HH.ClassName "before:bg-blue-88"
  , HH.ClassName "before:no-content"
  , HH.ClassName "before:rounded-full"
  , HH.ClassName "before:shadow"
  ]

radio
  :: ∀ p i
   . Array (HH.IProp HTMLlabel i)
  -> Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
radio iprops inprops html =
  HH.label
    ( [ HP.classes labelClasses ] <&> iprops )
    ( [ HH.input
        ( [ HP.classes inputClasses
          , HP.type_ InputRadio
          ] <&> inprops
        )
      , HH.span [ HP.classes radioClasses ] []
      ]
      <> html
    )

radio_
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
radio_ = radio []
