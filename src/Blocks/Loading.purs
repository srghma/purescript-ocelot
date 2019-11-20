module Ocelot.Block.Loading where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

type HTMLsvg = Interactive ()

svg :: forall p i. Node HTMLsvg p i
svg = element (ElemName "svg")

viewBox :: forall r i. Number -> Number -> Number -> Number -> IProp (viewBox :: String | r) i
viewBox x y w h = attr (AttrName "viewBox")
  (foldl showIntercalateSpace {init: true, val: ""} [x, y, w, h])
  where
    showIntercalateSpace acc next =
      if acc.init
        then { init: false, val: show next }
        else acc { val = acc.val <> " " <> show next }

type HTMLcircle = Interactive ()

circle :: forall w i. Leaf HTMLcircle w i
circle props = element (ElemName "circle") props []

cx :: forall r i. Number -> IProp (cx :: String | r) i
cx = attr (AttrName "cx") <<< show

cy :: forall r i. Number -> IProp (cy :: String | r) i
cy = attr (AttrName "cy") <<< show

r :: forall r i. Number -> IProp (r :: String | r) i
r = attr (AttrName "r") <<< show

fillNone :: forall r i. Number -> IProp (fill :: String | r) i
fillNone = attr (AttrName "fill") "none"

strokeWidth :: forall r i. Number -> IProp (strokeWidth :: String | r) i
strokeWidth = attr (AttrName "stroke-width") <<< show

strokeMiterLimt :: forall r i. Number -> IProp ("stroke-miterLimit" :: String | r) i
strokeMiterLimt = attr (AttrName "stroke-width") <<< show

spinner :: ∀ p i. Array (HH.IProp HTMLdiv i) -> HH.HTML p i
spinner props =
  HH.div
    ( [ HP.class_ $ HH.ClassName "loader" ] <&> props )
    [ svg
      [ HP.class $ HH.ClassName "circular"
      , viewBox 25 25 50 50
      ]
      [ circle
        [ HP.class_ $ HH.className "path"
        , cx 50, cy 50, r 20, fillNone, strokeWidth 4, strokeMiterLimit 10
        ]
      ]
    ]

spinner_ :: ∀ p i. HH.HTML p i
spinner_ = spinner []
