module UIGuide.Block.Backdrop where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties (joinClasses)
import TailwindClasses as TailwindClasses

backdropClasses :: Array HH.ClassName
backdropClasses =
  [ TailwindClasses.p_6
  , TailwindClasses.flex
  , TailwindClasses.flex_1
  ]

backdropDefaultClasses :: Array HH.ClassName
backdropDefaultClasses =
  backdropClasses
    <> [ TailwindClasses.bg_gray_95
      ]

backdropWhiteClasses :: Array HH.ClassName
backdropWhiteClasses =
  backdropClasses
    <> [ TailwindClasses.bg_white
      ]

backdropDarkClasses :: Array HH.ClassName
backdropDarkClasses =
  backdropClasses
    <> [ TailwindClasses.bg_black
      , TailwindClasses.text_gray_200
      ]

contentClasses :: Array HH.ClassName
contentClasses =
  [ TailwindClasses.flex_1
  , TailwindClasses.mx_6
  , TailwindClasses.mt_6
  ]

backdrop ::
  ∀ p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
backdrop iprops html =
  HH.div
    (joinClasses $ [ HP.classes backdropDefaultClasses ] <> iprops)
    html

backdrop_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
backdrop_ = backdrop []

backdropWhite ::
  ∀ p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
backdropWhite iprops html =
  HH.div
    (joinClasses $ [ HP.classes backdropWhiteClasses ] <> iprops)
    html

backdropWhite_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
backdropWhite_ = backdropWhite []

backdropDark ::
  ∀ p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
backdropDark iprops html =
  HH.div
    (joinClasses $ [ HP.classes backdropDarkClasses ] <> iprops)
    html

backdropDark_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
backdropDark_ = backdropDark []

content ::
  ∀ p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
content iprops html =
  HH.div
    (joinClasses $ [ HP.classes contentClasses ] <> iprops)
    html

content_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
content_ = content []
