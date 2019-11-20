module Ocelot.Block.Loading where

import Prelude

import DOM.HTML.Indexed (HTMLdiv, Interactive)
import Data.Foldable (foldl)
import Halogen (AttrName(..), ElemName(..))
import Halogen.HTML (IProp, Leaf, Node, attr, element)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

type HTMLsvg = Interactive ( viewBox :: String )

svg :: forall p i. Node HTMLsvg p i
svg = element (ElemName "svg")

viewBox :: forall r i. Int -> Int -> Int -> Int -> IProp (viewBox :: String | r) i
viewBox x y w h = attr (AttrName "viewBox")
  (foldl showIntercalateSpace {init: true, val: ""} [x, y, w, h]).val
  where
    showIntercalateSpace acc next =
      if acc.init
        then { init: false, val: show next }
        else acc { val = acc.val <> " " <> show next }

type HTMLcircle = Interactive
  ( cx :: String
  , cy :: String
  , r :: String
  , fill :: String
  , strokeWidth :: String
  , strokeMiterLimit :: String
  )

circle :: forall w i. Leaf HTMLcircle w i
circle props = element (ElemName "circle") props []

cx :: forall r i. Int -> IProp (cx :: String | r) i
cx = attr (AttrName "cx") <<< show

cy :: forall r i. Int -> IProp (cy :: String | r) i
cy = attr (AttrName "cy") <<< show

r :: forall rest i. Int -> IProp (r :: String | rest) i
r = attr (AttrName "r") <<< show

fillNone :: forall r i. IProp (fill :: String | r) i
fillNone = attr (AttrName "fill") "none"

strokeWidth :: forall r i. Int -> IProp (strokeWidth :: String | r) i
strokeWidth = attr (AttrName "stroke-width") <<< show

strokeMiterLimit :: forall r i. Int -> IProp (strokeMiterLimit :: String | r) i
strokeMiterLimit = attr (AttrName "stroke-width") <<< show

spinner :: ∀ p i. Array (HH.IProp HTMLdiv i) -> HH.HTML p i
spinner props =
  HH.div
    ( [ HP.class_ $ HH.ClassName "loader" ] <&> props )
    [ svg
      [ HP.class_ $ HH.ClassName "circular"
      , viewBox 25 25 50 50
      ]
      [ circle
        [ HP.class_ $ HH.ClassName "path"
        , cx 50, cy 50, r 20, fillNone, strokeWidth 4, strokeMiterLimit 10
        ]
      ]
    ]

spinner_ :: ∀ p i. HH.HTML p i
spinner_ = spinner []
