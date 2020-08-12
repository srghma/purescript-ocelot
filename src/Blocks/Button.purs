module Ocelot.Block.Button where

import Prelude

import DOM.HTML.Indexed (HTMLbutton, HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))
import TailwindClasses as TailwindClasses
import TailwindClasses.Active as TailwindClasses.Active
import TailwindClasses.Hover as TailwindClasses.Hover
import TailwindClasses.Disabled as TailwindClasses.Disabled

buttonSharedClasses :: Array HH.ClassName
buttonSharedClasses =
  [ TailwindClasses.outline_none
  , TailwindClasses.px_4
  , TailwindClasses.py_2
  , TailwindClasses.border_b
  , TailwindClasses.cursor_pointer
  , TailwindClasses.Active.border_t
  , TailwindClasses.Disabled.opacity_50
  , TailwindClasses.Disabled.cursor_default
  ]

buttonMainClasses :: Array HH.ClassName
buttonMainClasses = buttonSharedClasses <>
    [ TailwindClasses.rounded
    ]

buttonClasses :: Array HH.ClassName
buttonClasses =
  [ TailwindClasses.bg_gray_50_a20
  , TailwindClasses.border_gray_50_a20
  , TailwindClasses.Focus.bg_gray_50_a30
  , TailwindClasses.text_black_20
  ]

buttonPrimaryClasses :: Array HH.ClassName
buttonPrimaryClasses =
  [ TailwindClasses.bg_blue_88
  , TailwindClasses.border_blue_88
  , TailwindClasses.Focus.bg_blue_82
  , TailwindClasses.text_white
  ]

buttonDarkClasses :: Array HH.ClassName
buttonDarkClasses =
  [ TailwindClasses.bg_gray_70_a30
  , TailwindClasses.border_gray_70_a30
  , TailwindClasses.Focus.bg_gray_70_a40
  , TailwindClasses.text_white
  ]

buttonClearClasses :: Array HH.ClassName
buttonClearClasses =
  [ TailwindClasses.bg_transparent
  , TailwindClasses.border_transparent
  , TailwindClasses.text_gray_70
  , TailwindClasses.Hover.text_gray_70_a30
  , TailwindClasses.Focus.text_gray_70_a30
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
