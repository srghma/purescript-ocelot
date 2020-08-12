module Ocelot.Block.Badge where

import Prelude

import DOM.HTML.Indexed (HTMLspan)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties (joinClasses)
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
    <>
      [ TailwindClasses.w_8
      , TailwindClasses.h_8
      ]

badge ::
  ∀ p i.
  Array (IProp HTMLspan i) ->
  Array (HTML p i) ->
  HTML p i
badge iprops = HH.span $ joinClasses $ [ HP.classes badgeClasses ] <> iprops

badge_ ::
  ∀ p i.
  Array (HTML p i) ->
  HTML p i
badge_ = badge []

badgeSmallClasses :: Array HH.ClassName
badgeSmallClasses =
  baseClasses
    <>
      [ TailwindClasses.w_6
      , TailwindClasses.h_6
      , TailwindClasses.text_sm
      ]

badgeSmall ::
  ∀ p i.
  Array (IProp HTMLspan i) ->
  Array (HTML p i) ->
  HTML p i
badgeSmall iprops = HH.span $ joinClasses $ [ HP.classes badgeSmallClasses ] <> iprops

badgeSmall_ ::
  ∀ p i.
  Array (HTML p i) ->
  HTML p i
badgeSmall_ = badgeSmall []

badgeLargeClasses :: Array HH.ClassName
badgeLargeClasses =
  baseClasses
    <>
      [ TailwindClasses.w_12
      , TailwindClasses.h_12
      ]

badgeLarge ::
  ∀ p i.
  Array (IProp HTMLspan i) ->
  Array (HTML p i) ->
  HTML p i
badgeLarge iprops = HH.span $ joinClasses $ [ HP.classes badgeLargeClasses ] <> iprops

badgeLarge_ ::
  ∀ p i.
  Array (HTML p i) ->
  HTML p i
badgeLarge_ = badgeLarge []
