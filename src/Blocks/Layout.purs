module Ocelot.Block.Layout where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

popoverClasses :: Array HH.ClassName
popoverClasses =
  [ HH.ClassName "absolute"
  , HH.ClassName "shadow"
  , HH.ClassName "z-50"
  , HH.ClassName "border"
  , HH.ClassName "border-grey-90"
  , HH.ClassName "rounded"
  ]

popover
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
popover = blockBuilder HH.div popoverClasses

popover_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
popover_ = popover []

stickyClasses :: Array HH.ClassName
stickyClasses =
  [ HH.ClassName "fixed"
  , HH.ClassName "top-0"
  , HH.ClassName "inset-x-0"
  , HH.ClassName "w-full"
  , HH.ClassName "shadow-md"
  , HH.ClassName "z-60"
  ]

sticky
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
sticky = blockBuilder HH.div stickyClasses

sticky_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
sticky_ = sticky []

containerClasses :: Array HH.ClassName
containerClasses =
  [ HH.ClassName "container"
  , HH.ClassName "m-auto"
  ]

container
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
container = blockBuilder HH.div containerClasses

container_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
container_ = container []

sectionClasses :: Array HH.ClassName
sectionClasses =
  [ HH.ClassName "my-8"
  ]

section
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
section = blockBuilder HH.div sectionClasses

section_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
section_ = section []

gridClasses :: Array HH.ClassName
gridClasses =
  [ HH.ClassName "container"
  , HH.ClassName "m-auto"
  , HH.ClassName "p-8"
  , HH.ClassName "flex"
  ]

grid
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
grid = blockBuilder HH.div gridClasses

grid_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
grid_ = grid []

columnClasses :: Array HH.ClassName
columnClasses =
  [ HH.ClassName "flex-1"
  , HH.ClassName "p-8"
  ]

column
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
column = blockBuilder HH.div columnClasses

column_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
column_ = column []

mainClasses :: Array HH.ClassName
mainClasses =
  [ HH.ClassName "flex-3"
  , HH.ClassName "p-8"
  ]

main
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
main = blockBuilder HH.div mainClasses

main_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
main_ = main []

sideClasses :: Array HH.ClassName
sideClasses =
  [ HH.ClassName "flex-2"
  , HH.ClassName "p-8"
  ]

side
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
side = blockBuilder HH.div sideClasses

side_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
side_ = side []
