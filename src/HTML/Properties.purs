module Ocelot.HTML.Properties
  ( extract
  , appendIProps
  , (<&>)
  ) where

import Prelude

import Data.Array (elem, foldl, nubByEq)
import Data.Bifunctor (lmap, rmap)
import Data.String (Pattern(..), null, split)
import Data.String.CodeUnits (length, take, drop)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Core (Prop(..), PropValue)
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)

appendIProps
  :: ∀ r i
   . Array (HH.IProp ("class" :: String | r) i)
  -> Array (HH.IProp ("class" :: String | r) i)
  -> Array (HH.IProp ("class" :: String | r) i)
appendIProps ip ip' =
  iprops <> iprops' <> classNames
  where
    (Tuple classes iprops) = extract ip
    (Tuple classes' iprops') = extract ip'
    classNames = [HP.classes $ HH.ClassName <$> (classes' <> classes)]

infixr 5 appendIProps as <&>

extract
  :: ∀ r i
   . Array (HH.IProp ("class" :: String | r) i)
  -> Tuple (Array String) (Array (HH.IProp ("class" :: String | r) i))
extract =
  foldl f (Tuple [] [])
  where
    f acc (HP.IProp (Property "className" className)) = lmap (_ <> [coerceClassName className]) acc
    f acc iprop = rmap (_ <> [iprop]) acc

    coerceClassName :: PropValue -> String
    coerceClassName = unsafeCoerce
