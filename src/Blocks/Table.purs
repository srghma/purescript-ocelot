module Ocelot.Block.Table where

import Prelude

import TailwindClasses as TailwindClasses
import DOM.HTML.Indexed (HTMLtable, HTMLtd, HTMLth, HTMLtr)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

tableClasses :: Array HH.ClassName
tableClasses =
  [ HH.ClassName "w-full"
  , HH.ClassName "text-left"
  , HH.ClassName "border-collapse"
  ]

table
  :: ∀ p i
   . Array (IProp HTMLtable i)
  -> Array (HTML p i)
  -> HTML p i
table = blockBuilder HH.table tableClasses

table_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
table_ = table []

row
  :: ∀ p i
   . Array (IProp HTMLtr i)
  -> Array (HTML p i)
  -> HTML p i
row = HH.tr

row_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
row_ = HH.tr_

headerClasses :: Array HH.ClassName
headerClasses =
  [ HH.ClassName "bg-grey-90"
  , HH.ClassName "py-4"
  , HH.ClassName "px-5"
  , HH.ClassName "font-medium"
  , HH.ClassName "text-black-20"
  ]

header
  :: ∀ p i
   . Array (IProp HTMLth i)
  -> Array (HTML p i)
  -> HTML p i
header = blockBuilder HH.th headerClasses

header_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
header_ = header []

cellClasses :: Array HH.ClassName
cellClasses =
  [ HH.ClassName "bg-white"
  , HH.ClassName "p-5"
  , HH.ClassName "min-h-20"
  , HH.ClassName "border-b"
  , HH.ClassName "border-grey-95"
  ]

cell
  :: ∀ p i
   . Array (IProp HTMLtd i)
  -> Array (HTML p i)
  -> HTML p i
cell = blockBuilder HH.td cellClasses

cell_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
cell_ = cell []
