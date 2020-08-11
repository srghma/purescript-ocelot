module Ocelot.Block.Format where

import Prelude

import TailwindClasses as TailwindClasses
import DOM.HTML.Indexed (HTMLh1, HTMLh2, HTMLh3, HTMLh4, HTMLp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

headingClasses :: Array HH.ClassName
headingClasses =
  [ HH.ClassName "mb-6"
  , HH.ClassName "text-3xl"
  , HH.ClassName "font-normal"
  , HH.ClassName "leading-loose"
  , TailwindClasses.flex
  , HH.ClassName "items-center"
  ]

headingDarkClasses :: Array HH.ClassName
headingDarkClasses = headingClasses <>

    [ HH.ClassName "text-white"
    ]

subHeadingClasses :: Array HH.ClassName
subHeadingClasses =
  [ HH.ClassName "text-xl"
  , HH.ClassName "font-medium"
  , HH.ClassName "leading-loose"
  , TailwindClasses.flex
  , HH.ClassName "items-center"
  , HH.ClassName "mb-6"
  ]

subHeadingDarkClasses :: Array HH.ClassName
subHeadingDarkClasses = subHeadingClasses <>

    [ HH.ClassName "text-white"
    ]

contentHeadingClasses :: Array HH.ClassName
contentHeadingClasses =
  [ HH.ClassName "mb-6"
  , HH.ClassName "text-lg"
  , HH.ClassName "font-normal"
  , HH.ClassName "leading-loose"
  , TailwindClasses.flex
  , HH.ClassName "items-center"
  ]

captionClasses :: Array HH.ClassName
captionClasses =
  [ HH.ClassName "block"
  , HH.ClassName "font-light"
  , HH.ClassName "mb-6"
  , HH.ClassName "text-grey-70"
  , HH.ClassName "text-sm"
  , HH.ClassName "tracking-wide"
  , HH.ClassName "uppercase"
  ]

linkClasses :: Array HH.ClassName
linkClasses =
  [ HH.ClassName "text-blue-75"
  , HH.ClassName "hover:text-blue-65"
  , HH.ClassName "no-underline"
  , HH.ClassName "font-medium"
  , HH.ClassName "cursor-pointer"
  ]

linkDarkClasses :: Array HH.ClassName
linkDarkClasses =
  [ HH.ClassName "text-grey-light"
  , HH.ClassName "hover:text-grey-lighter"
  , HH.ClassName "no-underline"
  , HH.ClassName "font-medium"
  , HH.ClassName "cursor-pointer"
  ]

mutedClasses :: Array HH.ClassName
mutedClasses =
  [ HH.ClassName "text-grey-50"
  ]

pClasses :: Array HH.ClassName
pClasses =
  [ HH.ClassName "mb-6"
  ]

heading
  :: ∀ p i
   . Array (HH.IProp HTMLh1 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
heading iprops =
  HH.h1
    ( [ HP.classes headingClasses ] <&> iprops )

heading_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
heading_ = heading []

headingDark
  :: ∀ p i
   . Array (HH.IProp HTMLh1 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
headingDark iprops =
  HH.h1
    ( [ HP.classes headingDarkClasses ] <&> iprops )

headingDark_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
headingDark_ = headingDark []

subHeading
  :: ∀ p i
   . Array (HH.IProp HTMLh2 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
subHeading iprops html =
  HH.h2
    ( [ HP.classes subHeadingClasses ] <&> iprops )
    html

subHeading_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
subHeading_ = subHeading []

subHeadingDark
  :: ∀ p i
   . Array (HH.IProp HTMLh2 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
subHeadingDark iprops =
  HH.h2
    ( [ HP.classes subHeadingDarkClasses ] <&> iprops )

subHeadingDark_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
subHeadingDark_ = subHeadingDark []

contentHeading
  :: ∀ p i
   . Array (HH.IProp HTMLh3 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
contentHeading iprops =
  HH.h3
    ( [ HP.classes contentHeadingClasses ] <&> iprops )

contentHeading_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
contentHeading_ = contentHeading []

caption
  :: ∀ p i
   . Array (HH.IProp HTMLh4 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
caption iprops =
  HH.h4
    ( [ HP.classes captionClasses ] <&> iprops )

caption_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
caption_ = caption []

p
  :: ∀ p i
   . Array (HH.IProp HTMLp i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
p iprops =
  HH.p
      ( [ HP.classes pClasses ] <&> iprops )

p_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
p_ = p []
