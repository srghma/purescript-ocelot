module Test.Main where

import Prelude

import Effect (Effect)
import Test.Ocelot.Data.Currency as Test.Ocelot.Data.Currency
import Test.Unit as Test.Unit
import Test.Unit.Main as Test.Unit.Main

main :: Effect Unit
main = Test.Unit.Main.runTest do
  Test.Unit.suite "Ocelet.Data.Currency" Test.Ocelot.Data.Currency.suite
