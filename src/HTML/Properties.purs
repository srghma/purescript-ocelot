module Ocelot.HTML.Properties
  ( joinClasses
  , appendIProps
  , (<&>)
  ) where

import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STA
import Data.Array.ST.Iterator as STAI
import Data.String as String
import Halogen.HTML as HH
import Halogen.HTML.Core (Prop(..), PropValue)
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)

joinClasses
  :: ∀ r i
   . Array (HH.IProp ("class" :: String | r) i)
  -> Array (HH.IProp ("class" :: String | r) i)
joinClasses xs =
  ST.run do
    classNames <- STA.empty
    otherProps <- STA.empty
    iter <- STAI.iterator (xs Array.!! _)
    STAI.iterate iter \iprop ->
      case iprop of
           HP.IProp (Property "className" className) -> void $ STA.push ((unsafeCoerce :: PropValue -> String) className) classNames
           _ -> void $ STA.push iprop otherProps
    (classNames' :: Array String) <- STA.unsafeFreeze classNames
    void $ STA.push (HP.IProp (Property "className" ((unsafeCoerce :: String -> PropValue) (String.joinWith " " classNames')))) otherProps
    allProps <- STA.unsafeFreeze $ otherProps
    pure $ allProps

appendIProps
  :: ∀ r i
   . Array (HH.IProp ("class" :: String | r) i)
  -> Array (HH.IProp ("class" :: String | r) i)
  -> Array (HH.IProp ("class" :: String | r) i)
appendIProps a b = joinClasses (a <> b)

infixr 5 appendIProps as <&>
