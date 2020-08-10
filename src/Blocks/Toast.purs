module Ocelot.Block.Toast where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Array (snoc)
import Data.Bifunctor (lmap, rmap)
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))
import Unsafe.Coerce (unsafeCoerce)

type Toast r = ( visible :: Boolean | r )

type HTMLtoast = Toast HTMLdiv

visible :: ∀ r i. Boolean -> HP.IProp ( visible :: Boolean | r ) i
visible = HP.prop (HH.PropName "visible")

-- Necessary for centering the toast
toastContainerClasses :: Array HH.ClassName
toastContainerClasses =
  [ HH.ClassName "flex"
  , HH.ClassName "transition-1/4-in"
  , HH.ClassName "transition-1/2-out"
  , HH.ClassName "items-center"
  , HH.ClassName "fixed"
  , HH.ClassName "left-0"
  , HH.ClassName "right-0"
  , HH.ClassName "bottom-0"
  ]

containerVisibleClasses :: Array HH.ClassName
containerVisibleClasses =
  [ HH.ClassName "mb-8"
  ]

containerClosedClasses :: Array HH.ClassName
containerClosedClasses =
  [ HH.ClassName "-mb-40"
  ]

toastClasses :: Array HH.ClassName
toastClasses =
  [ HH.ClassName "shadow-md"
  , HH.ClassName "p-4"
  , HH.ClassName "ml-auto"
  , HH.ClassName "mr-auto"
  , HH.ClassName "items-center"
  , HH.ClassName "border"
  , HH.ClassName "border-grey-80"
  , HH.ClassName "bg-white"
  , HH.ClassName "rounded"
  , HH.ClassName "flex"
  ]

toast
  :: ∀ p i
   . Array (HH.IProp HTMLtoast i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
toast iprops html =
  HH.div
  [ HP.classes $ toastContainerClasses <> containerClasses' ]
  [ HH.div
    ( [ HP.classes toastClasses ] <&> iprops' )
    html
  ]
  where
    Tuple visible iprops' = pullVisibleProp iprops
    containerClasses' =
      if visible
        then containerVisibleClasses
        else containerClosedClasses


pullVisibleProp
  :: ∀ r i
   . Array (HH.IProp ( visible :: Boolean | r ) i)
  -> Tuple Boolean (Array (HH.IProp r i))
pullVisibleProp = foldr f (Tuple false [])
  where
    f (HP.IProp (HC.Property "visible" x)) =
      lmap $ const $ coerceExpanded x
    f iprop = rmap $ (flip snoc) $ coerceR iprop

    coerceExpanded :: HC.PropValue -> Boolean
    coerceExpanded = unsafeCoerce

    coerceR :: HH.IProp ( visible :: Boolean | r ) i -> HH.IProp r i
    coerceR = unsafeCoerce
