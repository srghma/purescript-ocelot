module Ocelot.Block.TabControl where

import Prelude

import TailwindClasses as TailwindClasses
import DOM.HTML.Indexed (HTMLdiv, HTMLa)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
import Ocelot.HTML.Properties ((<&>))

type Tab i page =
  { name :: String
  , props :: Array (HH.IProp HTMLa i)
  , page :: page
  , errors :: Int
  }

type TabConfig tabType page =
  { tabs :: Array (tabType page)
  , activePage :: page
  }

type IsActive = Boolean

outerClasses :: Array HH.ClassName
outerClasses =
  [ HH.ClassName "bg-black-10"
  , HH.ClassName "w-full"
  ]

innerClasses :: Array HH.ClassName
innerClasses =
  [ HH.ClassName "container"
  , HH.ClassName "items-end"
  , HH.ClassName "mx-auto"
  , TailwindClasses.flex
  , HH.ClassName "h-16"
  ]

tabClasses :: Array HH.ClassName
tabClasses =
  [ HH.ClassName "pt-5"
  , HH.ClassName "pb-6"
  , HH.ClassName "inline-flex"
  , HH.ClassName "no-underline"
  ]

activeTabClasses :: Array HH.ClassName
activeTabClasses =
  [ HH.ClassName "border-b-2"
  , HH.ClassName "border-blue-88"
  , HH.ClassName "text-white"
  ]

inactiveTabClasses :: Array HH.ClassName
inactiveTabClasses =
  [ HH.ClassName "border-b-2"
  , HH.ClassName "border-black-10"
  , HH.ClassName "hover:border-blue-88"
  , HH.ClassName "hover:text-white"
  , HH.ClassName "text-grey-70"
  ]

tabTextClasses :: Array HH.ClassName
tabTextClasses =
  [ HH.ClassName "text-sm"
  , HH.ClassName "tracking-wide"
  , HH.ClassName "uppercase"
  , HH.ClassName "bold"
  , HH.ClassName "inline-flex"
  , HH.ClassName "self-end"
  ]

errorIconClasses :: Array HH.ClassName
errorIconClasses =
  [ HH.ClassName "text-2xl"
  , HH.ClassName "text-red"
  , HH.ClassName "mr-1"
  , HH.ClassName "inline-flex"
  , HH.ClassName "align-bottom"
  , HH.ClassName "my-px"
  ]

tabControl
  :: ∀ p i page
   . Eq page
  => TabConfig (Tab i) page
  -> Array (HH.IProp HTMLdiv i)
  -> HH.HTML p i
tabControl { tabs, activePage } props =
  HH.div
    [ HP.classes outerClasses ]
    [ HH.ul
      ( [ HP.classes innerClasses ] <&> props )
      $ singleTab activePage <$> tabs
    ]

tabControl_
  :: ∀ p i page
   . Eq page
  => TabConfig (Tab i) page
  -> HH.HTML p i
tabControl_ config = tabControl config []

singleTab
  :: ∀ p i page
   . Eq page
  => page
  -> Tab i page
  -> HH.HTML p i
singleTab activePage tab =
  HH.li
    [ HP.class_ $ HH.ClassName "mr-12" ]
    [ HH.a
      ( tab.props
        <> [ HP.classes $ tabClasses <> conditionalTabClasses isActive ]
      )
      ( errorIcon
        <>
        [ HH.span
          [ HP.classes tabTextClasses ]
          [ HH.text tab.name ]
        ]
      )
    ]
  where
    isActive :: Boolean
    isActive = tab.page == activePage

    errorIcon :: Array (HH.HTML p i)
    errorIcon
      | tab.errors > 0 && isActive == false =
        [ Icon.error
          [ HP.classes $ errorIconClasses ]
        ]
      | otherwise = []

    conditionalTabClasses :: IsActive -> Array HH.ClassName
    conditionalTabClasses true = activeTabClasses
    conditionalTabClasses false = inactiveTabClasses
