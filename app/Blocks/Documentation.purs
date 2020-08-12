module UIGuide.Block.Documentation where

import Prelude
import TailwindClasses as TailwindClasses
import DOM.HTML.Indexed (HTMLdiv, HTMLsection, HTMLheader)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format as Format

type DocumentationConfig
  = { header :: String
    , subheader :: String
    }

blockClasses :: Array HH.ClassName
blockClasses =
  [ TailwindClasses.my_20
  ]

introClasses :: Array HH.ClassName
introClasses =
  [ TailwindClasses.my_12
  ]

headingClasses :: Array HH.ClassName
headingClasses =
  [ HH.ClassName "w-1/2"
  ]

subHeadingClasses :: Array HH.ClassName
subHeadingClasses =
  [ HH.ClassName "w-1/2"
  , TailwindClasses.font_light
  , TailwindClasses.text_gray_50
  ]

calloutClasses :: Array HH.ClassName
calloutClasses =
  [ TailwindClasses.border_dotted
  , TailwindClasses.border
  , TailwindClasses.rounded
  , TailwindClasses.flex
  , TailwindClasses.items_stretch
  , TailwindClasses.my_6
  ]

callout ::
  ∀ p i.
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
callout iprops html =
  HH.div
    ([ HP.classes calloutClasses ] <> iprops)
    html

callout_ ::
  ∀ p i.
  Array (HH.HTML p i) ->
  HH.HTML p i
callout_ = callout []

intro ::
  ∀ p i.
  DocumentationConfig ->
  Array (HH.IProp HTMLheader i) ->
  HH.HTML p i
intro config iprops =
  HH.header
    ([ HP.classes introClasses ] <> iprops)
    [ Format.heading
        [ HP.classes headingClasses ]
        [ HH.text config.header ]
    , Format.subHeading
        [ HP.classes subHeadingClasses ]
        [ HH.text config.subheader ]
    ]

intro_ ::
  ∀ p i.
  DocumentationConfig ->
  HH.HTML p i
intro_ config = intro config []

customBlock ::
  ∀ p i.
  DocumentationConfig ->
  Array (HH.IProp HTMLsection i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
customBlock config iprops html =
  HH.section
    ([ HP.classes blockClasses ] <> iprops)
    [ intro_ config
    , HH.div_
        html
    ]

customBlock_ ::
  ∀ p i.
  DocumentationConfig ->
  Array (HH.HTML p i) ->
  HH.HTML p i
customBlock_ config = customBlock config []

block ::
  ∀ p i.
  DocumentationConfig ->
  Array (HH.IProp HTMLsection i) ->
  Array (HH.HTML p i) ->
  HH.HTML p i
block config iprops html = customBlock config iprops [ callout_ html ]

block_ ::
  ∀ p i.
  DocumentationConfig ->
  Array (HH.HTML p i) ->
  HH.HTML p i
block_ config = block config []
