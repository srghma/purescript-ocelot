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
  , HH.ClassName "justify-center"
  , HH.ClassName "items-center"
  , HH.ClassName "bg-blue-88"
  , HH.ClassName "text-white"
  ]

badgeClasses :: Array HH.ClassName
badgeClasses =
  baseClasses
    <> [ HH.ClassName "w-8"
      , HH.ClassName "h-8"
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
    <> [ HH.ClassName "w-6"
      , HH.ClassName "h-6"
      , HH.ClassName "text-sm"
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
    <> [ HH.ClassName "w-12"
      , HH.ClassName "h-12"
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
