module Ocelot.Block.Badge where

import Prelude

import DOM.HTML.Indexed (HTMLspan)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

baseClasses :: Array HH.ClassName
baseClasses =
  [ HH.ClassName "rounded-full"
  , HH.ClassName "relative"
  , HH.ClassName "before:no-content"
  , HH.ClassName "before:w-full"
  , HH.ClassName "before:h-full"
  , HH.ClassName "before:absolute"
  , HH.ClassName "before:top-0"
  , HH.ClassName "before:left-0"
  , HH.ClassName "flex"
  , HH.ClassName "justify-center"
  , HH.ClassName "items-center"
  , HH.ClassName "bg-blue-88"
  , HH.ClassName "text-white"
  ]

badgeClasses :: Array HH.ClassName
badgeClasses = baseClasses <>
  [ HH.ClassName "w-8"
  , HH.ClassName "h-8"
  ]

badge
  :: ∀ p i
   . Array (IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
badge = blockBuilder HH.span badgeClasses

badge_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
badge_ = badge []

badgeSmallClasses :: Array HH.ClassName
badgeSmallClasses = baseClasses <>
  [ HH.ClassName "w-6"
  , HH.ClassName "h-6"
  , HH.ClassName "text-sm"
  ]

badgeSmall
  :: ∀ p i
   . Array (IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
badgeSmall = blockBuilder HH.span badgeSmallClasses

badgeSmall_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
badgeSmall_ = badgeSmall []

badgeLargeClasses :: Array HH.ClassName
badgeLargeClasses = baseClasses <>
  [ HH.ClassName "w-12"
  , HH.ClassName "h-12"
  ]

badgeLarge
  :: ∀ p i
   . Array (IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
badgeLarge = blockBuilder HH.span badgeLargeClasses

badgeLarge_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
badgeLarge_ = badgeLarge []
