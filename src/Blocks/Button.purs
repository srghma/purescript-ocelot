module Ocelot.Block.Button where

import Prelude

import DOM.HTML.Indexed (HTMLbutton, HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))
import TailwindClasses as TailwindClasses
import TailwindClasses.Active as TailwindClasses.Active
import TailwindClasses.Hover as TailwindClasses.Hover

buttonSharedClasses :: Array HH.ClassName
buttonSharedClasses =
  [ TailwindClasses.outline_none
  , TailwindClasses.px_4
  , TailwindClasses.py_2
  , HH.ClassName "border-b"
  , HH.ClassName "cursor-pointer"
  , TailwindClasses.Active.border_t
  , HH.ClassName "disabled:opacity-50"
  , HH.ClassName "disabled:cursor-default"
  ]

buttonMainClasses :: Array HH.ClassName
buttonMainClasses = buttonSharedClasses <>
    [ TailwindClasses.rounded
    ]

buttonClasses :: Array HH.ClassName
buttonClasses =
  [ HH.ClassName "bg-gray-50-a20"
  , HH.ClassName "border-gray-50-a20"
  , HH.ClassName "hover:!disabled:bg-gray-50-a30"
  , HH.ClassName "focus:bg-gray-50-a30"
  , TailwindClasses.text_black_20
  ]

buttonPrimaryClasses :: Array HH.ClassName
buttonPrimaryClasses =
  [ TailwindClasses.bg_blue_88
  , TailwindClasses.border_blue_88
  , HH.ClassName "hover:!disabled:bg-blue-82"
  , HH.ClassName "focus:bg-blue-82"
  , TailwindClasses.text_white
  ]

buttonDarkClasses :: Array HH.ClassName
buttonDarkClasses =
  [ HH.ClassName "bg-gray-70-a30"
  , HH.ClassName "border-gray-70-a30"
  , HH.ClassName "hover:!disabled:bg-gray-70-a40"
  , HH.ClassName "focus:bg-gray-70-a40"
  , TailwindClasses.text_white
  ]

buttonClearClasses :: Array HH.ClassName
buttonClearClasses =
  [ TailwindClasses.bg_transparent
  , TailwindClasses.border_transparent
  , TailwindClasses.text_gray_70
  , HH.ClassName "hover:text-gray-70-a30"
  , HH.ClassName "focus:text-gray-70-a30"
  ]

buttonGroupClasses :: Array HH.ClassName
buttonGroupClasses =
  [ TailwindClasses.flex
  , TailwindClasses.items_center
  ]

centerClasses :: Array HH.ClassName
centerClasses =
  [ TailwindClasses.mr_px
  ]

leftClasses :: Array HH.ClassName
leftClasses =
  [ TailwindClasses.mr_px
  , TailwindClasses.rounded_l
  ]

rightClasses :: Array HH.ClassName
rightClasses =
  [ TailwindClasses.rounded_r
  ]

buttonBuilder
  :: ∀ p i
   . Array HH.ClassName
  -> Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonBuilder classes iprops =
  HH.button
    ( [ HP.classes $ buttonMainClasses <> classes ] <&> iprops )

button
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
button =
  buttonBuilder buttonClasses

button_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
button_ = button []

buttonPrimary
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimary =
  buttonBuilder buttonPrimaryClasses

buttonPrimary_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimary_ = buttonPrimary []

buttonDark
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonDark =
  buttonBuilder buttonDarkClasses

buttonDark_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonDark_ = buttonDark []

buttonClear
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonClear =
  buttonBuilder buttonClearClasses

buttonClear_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonClear_ = buttonClear []

buttonGroup
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonGroup iprops =
  HH.div
    ( [ HP.classes buttonGroupClasses ] <&> iprops )

buttonGroup_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonGroup_ = buttonGroup []

buttonGroupBuilder
  :: ∀ p i
   . Array HH.ClassName
  -> Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonGroupBuilder classes iprops =
  HH.button
    ( [ HP.classes $ buttonSharedClasses <> classes ] <&> iprops )

buttonCenter
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonCenter =
  buttonGroupBuilder $ buttonClasses <> centerClasses

buttonCenter_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonCenter_ = buttonCenter []

buttonPrimaryCenter
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryCenter =
  buttonGroupBuilder $ buttonPrimaryClasses <> centerClasses

buttonPrimaryCenter_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryCenter_ = buttonPrimaryCenter []

buttonDarkCenter
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkCenter =
  buttonGroupBuilder $ buttonDarkClasses <> centerClasses

buttonDarkCenter_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkCenter_ = buttonDarkCenter []

buttonClearCenter
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearCenter =
  buttonGroupBuilder $ buttonClearClasses <> centerClasses

buttonLeft
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonLeft =
  buttonGroupBuilder $ buttonClasses <> leftClasses

buttonLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonLeft_ = buttonLeft []

buttonPrimaryLeft
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryLeft =
  buttonGroupBuilder $ buttonPrimaryClasses <> leftClasses

buttonPrimaryLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryLeft_ = buttonPrimaryLeft []

buttonDarkLeft
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkLeft =
  buttonGroupBuilder $ buttonDarkClasses <> leftClasses

buttonDarkLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkLeft_ = buttonDarkLeft []

buttonClearLeft
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearLeft =
  buttonGroupBuilder $ buttonClearClasses <> leftClasses

buttonClearLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearLeft_ = buttonClearLeft []

buttonRight
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonRight =
  buttonGroupBuilder $ buttonClasses <> rightClasses

buttonRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonRight_ = buttonRight []

buttonPrimaryRight
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryRight =
  buttonGroupBuilder $ buttonPrimaryClasses <> rightClasses

buttonPrimaryRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonPrimaryRight_ = buttonPrimaryRight []

buttonDarkRight
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkRight =
  buttonGroupBuilder $ buttonDarkClasses <> rightClasses

buttonDarkRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonDarkRight_ = buttonDarkRight []

buttonClearRight
  :: ∀ p i
   . Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearRight =
  buttonGroupBuilder $ buttonClearClasses <> rightClasses

buttonClearRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
buttonClearRight_ = buttonClearRight []
