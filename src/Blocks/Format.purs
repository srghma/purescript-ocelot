module Ocelot.Block.Format where

import Prelude
import Ocelot.HTML.Properties
import TailwindClasses as TailwindClasses
import TailwindClasses.Hover as TailwindClasses.Hover
import DOM.HTML.Indexed (HTMLh1, HTMLh2, HTMLh3, HTMLh4, HTMLp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

headingClasses :: Array HH.ClassName
headingClasses =
  [ TailwindClasses.mb_6
  , TailwindClasses.text_3xl
  , TailwindClasses.font_normal
  , TailwindClasses.leading_loose
  , TailwindClasses.flex
  , TailwindClasses.items_center
  ]

headingDarkClasses :: Array HH.ClassName
headingDarkClasses =
  headingClasses
    <>
      [ TailwindClasses.text_white
      ]

subHeadingClasses :: Array HH.ClassName
subHeadingClasses =
  [ TailwindClasses.text_xl
  , TailwindClasses.font_medium
  , TailwindClasses.leading_loose
  , TailwindClasses.flex
  , TailwindClasses.items_center
  , TailwindClasses.mb_6
  ]

subHeadingDarkClasses :: Array HH.ClassName
subHeadingDarkClasses =
  subHeadingClasses
    <> [ TailwindClasses.text_white
      ]

contentHeadingClasses :: Array HH.ClassName
contentHeadingClasses =
  [ TailwindClasses.mb_6
  , TailwindClasses.text_lg
  , TailwindClasses.font_normal
  , TailwindClasses.leading_loose
  , TailwindClasses.flex
  , TailwindClasses.items_center
  ]

captionClasses :: Array HH.ClassName
captionClasses =
  [ TailwindClasses.block
  , TailwindClasses.font_light
  , TailwindClasses.mb_6
  , TailwindClasses.text_gray_70
  , TailwindClasses.text_sm
  , TailwindClasses.tracking_wide
  , TailwindClasses.uppercase
  ]

linkClasses :: Array HH.ClassName
linkClasses =
  [ TailwindClasses.text_blue_75
  , TailwindClasses.Hover.text_blue_65
  , TailwindClasses.no_underline
  , TailwindClasses.font_medium
  , TailwindClasses.cursor_pointer
  ]

linkDarkClasses :: Array HH.ClassName
linkDarkClasses =
  [ TailwindClasses.text_gray_300
  , TailwindClasses.Hover.text_gray_200
  , TailwindClasses.no_underline
  , TailwindClasses.font_medium
  , TailwindClasses.cursor_pointer
  ]

mutedClasses :: Array HH.ClassName
mutedClasses =
  [ TailwindClasses.text_gray_50
  ]

pClasses :: Array HH.ClassName
pClasses =
  [ TailwindClasses.mb_6
  ]

heading ::
  ∀ p i.
  Array (HH.IProp HTMLh1 i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
heading iprops =
  HH.h1
    ([ HP.classes headingClasses ] <&> iprops)

heading_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
heading_ = heading []

headingDark ::
  ∀ p i.
  Array (HH.IProp HTMLh1 i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
headingDark iprops =
  HH.h1
    ([ HP.classes headingDarkClasses ] <&> iprops)

headingDark_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
headingDark_ = headingDark []

subHeading ::
  ∀ p i.
  Array (HH.IProp HTMLh2 i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
subHeading iprops html =
  HH.h2
    ([ HP.classes subHeadingClasses ] <&> iprops)
    html

subHeading_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
subHeading_ = subHeading []

subHeadingDark ::
  ∀ p i.
  Array (HH.IProp HTMLh2 i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
subHeadingDark iprops =
  HH.h2
    ([ HP.classes subHeadingDarkClasses ] <&> iprops)

subHeadingDark_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
subHeadingDark_ = subHeadingDark []

contentHeading ::
  ∀ p i.
  Array (HH.IProp HTMLh3 i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
contentHeading iprops =
  HH.h3
    ([ HP.classes contentHeadingClasses ] <&> iprops)

contentHeading_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
contentHeading_ = contentHeading []

caption ::
  ∀ p i.
  Array (HH.IProp HTMLh4 i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
caption iprops =
  HH.h4
    ([ HP.classes captionClasses ] <&> iprops)

caption_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
caption_ = caption []

p ::
  ∀ p i.
  Array (HH.IProp HTMLp i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
p iprops =
  HH.p
    ([ HP.classes pClasses ] <&> iprops)

p_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
p_ = p []
