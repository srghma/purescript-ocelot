module Ocelot.Block.Badge where

import Prelude
import DOM.HTML.Indexed (HTMLspan)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
import TailwindClasses as TailwindClasses

baseClasses :: Array HH.ClassName
baseClasses =
  [ TailwindClasses.rounded_full
  , TailwindClasses.relative
  , TailwindClasses.flex
  , TailwindClasses.justify_center
  , TailwindClasses.items_center
  , TailwindClasses.bg_blue_88
  , TailwindClasses.text_white
  ]

badgeClasses :: Array HH.ClassName
badgeClasses =
  baseClasses
    <> [ TailwindClasses.w_8
      , TailwindClasses.h_8
      ]

badge ::
  ∀ p i.
  Array (IProp HTMLspan i) ->
  Array (HTML p i) ->
  HTML p i
badge props = HH.span $ [ HP.classes badgeClasses ] <> props

badge_ ::
  ∀ p i.
  Array (HTML p i) ->
  HTML p i
badge_ = badge []

badgeSmallClasses :: Array HH.ClassName
badgeSmallClasses =
  baseClasses
    <> [ TailwindClasses.w_6
      , TailwindClasses.h_6
      , TailwindClasses.text_sm
      ]

badgeSmall ::
  ∀ p i.
  Array (IProp HTMLspan i) ->
  Array (HTML p i) ->
  HTML p i
badgeSmall props = HH.span $ [ HP.classes badgeSmallClasses ] <> props

badgeSmall_ ::
  ∀ p i.
  Array (HTML p i) ->
  HTML p i
badgeSmall_ = badgeSmall []

badgeLargeClasses :: Array HH.ClassName
badgeLargeClasses =
  baseClasses
    <> [ TailwindClasses.w_12
      , TailwindClasses.h_12
      ]

badgeLarge ::
  ∀ p i.
  Array (IProp HTMLspan i) ->
  Array (HTML p i) ->
  HTML p i
badgeLarge props = HH.span $ [ HP.classes badgeLargeClasses ] <> props

badgeLarge_ ::
  ∀ p i.
  Array (HTML p i) ->
  HTML p i
badgeLarge_ = badgeLarge []
