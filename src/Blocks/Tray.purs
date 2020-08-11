module Ocelot.Block.Tray where

import Prelude

import TailwindClasses as TailwindClasses
import Data.Array (foldr, snoc)
import Data.Bifunctor (lmap, rmap)
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))
import Unsafe.Coerce (unsafeCoerce)

type Tray r = ( open :: Boolean | r )

type HTMLtray = Tray HTMLdiv

open :: ∀ r i. Boolean -> HP.IProp ( open :: Boolean | r ) i
open = HP.prop (HH.PropName "open")

trayClasses :: Array HH.ClassName
trayClasses =
  [ HH.ClassName "fixed"
  , HH.ClassName "bg-white"
  , HH.ClassName "p-6"
  , HH.ClassName "bottom-0"
  , HH.ClassName "left-0"
  , HH.ClassName "shadow"
  , HH.ClassName "border-t"
  , HH.ClassName "border-grey-90"
  , HH.ClassName "transition-1/4-out"
  , HH.ClassName "w-full"
  , TailwindClasses.flex
  , HH.ClassName "items-center"
  ]

trayOpenClasses :: Array HH.ClassName
trayOpenClasses =
  [ HH.ClassName "bottom-0"
  ]

trayClosedClasses :: Array HH.ClassName
trayClosedClasses =
  [ HH.ClassName "-bottom-40"
  ]

tray
  :: ∀ p i
   . Array (HH.IProp HTMLtray i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
tray iprops html = HH.div
  ( [ HP.classes (trayClasses <> trayClasses') ] <&> iprops' )
  html
  where
    Tuple open iprops' = pullOpenProp iprops

    trayClasses' =
      if open
        then trayOpenClasses
        else trayClosedClasses

pullOpenProp
  :: ∀ r i
   . Array (HH.IProp ( open :: Boolean | r ) i)
  -> Tuple Boolean (Array (HH.IProp r i))
pullOpenProp = foldr f (Tuple false [])
  where
    f (HP.IProp (HC.Property "open" x)) =
      lmap $ const $ coerceExpanded x
    f iprop = rmap $ (flip snoc) $ coerceR iprop

    coerceExpanded :: HC.PropValue -> Boolean
    coerceExpanded = unsafeCoerce

    coerceR :: HH.IProp ( open :: Boolean | r ) i -> HH.IProp r i
    coerceR = unsafeCoerce
