module Ocelot.Block.ItemContainer where

import Prelude

import DOM.HTML.Indexed (HTMLbutton, HTMLdiv)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Fuzzy (Fuzzy(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object (lookup)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
import Ocelot.HTML.Properties (IProp, css, (<&>))
import Select as Select
import Select.Setters as Setters

menuClasses :: Array HH.ClassName
menuClasses =
  [ HH.ClassName "bg-white"
  , HH.ClassName "text-black-20"
  , HH.ClassName "border"
  , HH.ClassName "rounded"
  , HH.ClassName "shadow"
  , HH.ClassName "absolute"
  , HH.ClassName "z-60"
  , HH.ClassName "min-w-50"
  ]

dropdownClasses :: Array HH.ClassName
dropdownClasses = menuClasses <>

    [ HH.ClassName "absolute"
    , HH.ClassName "top-full"
    , HH.ClassName "left-0"
    , HH.ClassName "max-h-160"
    , HH.ClassName "overflow-y-auto"
    ]

droprightClasses :: Array HH.ClassName
droprightClasses = menuClasses <>

    [ HH.ClassName "absolute"
    , HH.ClassName "top-0"
    , HH.ClassName "left-full"
    ]

baseClasses :: Array HH.ClassName
baseClasses =
  [ HH.ClassName "bg-white"
  , HH.ClassName "border-grey-80"
  , HH.ClassName "border-l-2"
  , HH.ClassName "border-r-2"
  , HH.ClassName "w-full"
  ]

selectionContainerClasses :: Array HH.ClassName
selectionContainerClasses = baseClasses <>

    [ HH.ClassName "border-t-2"
    ]

itemContainerClasses :: Array HH.ClassName
itemContainerClasses = baseClasses <>

    [ HH.ClassName "absolute"
    , HH.ClassName "shadow"
    , HH.ClassName "max-h-120"
    , HH.ClassName "overflow-y-auto"
    , HH.ClassName "z-50"
    , HH.ClassName "border-b-2"
    , HH.ClassName "top-full"
    , HH.ClassName "left-0"
    ]

ulClasses :: Array HH.ClassName
ulClasses =  []

liClasses :: Array HH.ClassName
liClasses =
  [ HH.ClassName "px-4"
  , HH.ClassName "py-2"
  , HH.ClassName "rounded-sm"
  , HH.ClassName "text-black-20"
  , HH.ClassName "group"
  , HH.ClassName "hover:bg-grey-97"
  , HH.ClassName "cursor-pointer"
  ]

selectionGroupClasses :: Array HH.ClassName
selectionGroupClasses =
  [ HH.ClassName "flex"
  , HH.ClassName "items-start"
  , HH.ClassName "justify-between"
  ]

buttonClasses :: Array HH.ClassName
buttonClasses =
  [ HH.ClassName "invisible"
  , HH.ClassName "text-grey-80"
  , HH.ClassName "hover:text-grey-70"
  , HH.ClassName "group-hover:visible"
  ]

dropdownButton
  :: ∀ p r i
   . (Array (IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i)
  -> Array (IProp r i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
dropdownButton button iprops html =
  button
    ( [ css "font-medium flex items-center" ] <&> iprops )
    $ html <> [ Icon.caratDown [ css "ml-3 text-xs" ] ]

dropdownContainer
  :: ∀ p action item
   . Array (HH.HTML p (Select.Action action))
  -> (item -> HH.PlainHTML)
  -> (item -> Boolean)
  -> Array item
  -> Maybe Int
  -> HH.HTML p (Select.Action action)
dropdownContainer addlHTML renderItem selected items hix =
  HH.div
    ( Setters.setContainerProps [ HP.classes dropdownClasses ] )
    ( addlHTML <> renderItems )
  where
    renderItems :: Array (HH.HTML p (Select.Action action))
    renderItems =
      [ HH.ul
        [ HP.classes ulClasses ]
        $ mapWithIndex renderItem' items
      ]

    renderItem' :: Int -> item -> HH.HTML p (Select.Action action)
    renderItem' idx item =
      dropdownItem HH.li
        ( Setters.setItemProps idx [] )
        [ HH.fromPlainHTML $ renderItem item ]
        ( selected item )
        ( hix == Just idx )

dropdownItem
  :: ∀ p r i
   . (Array (IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i)
  -> Array (IProp r i)
  -> Array (HH.HTML p i)
  -> Boolean
  -> Boolean
  -> HH.HTML p i
dropdownItem elem props html selected highlighted =
  elem
    ( props <&> [ HP.classes itemClasses ] )
    $ [ Icon.selected [ HP.classes checkmarkClass ] ] <> html
  where
    itemClasses :: Array HH.ClassName
    itemClasses =
      liClasses
      <> [ HH.ClassName "flex" ]
      <> ( if highlighted then [ HH.ClassName "bg-grey-lighter" ] else [] )
      <> if selected then [ HH.ClassName "font-medium" ] else []

    checkmarkClass :: Array HH.ClassName
    checkmarkClass =
      ( [ HH.ClassName "mr-2"
        , HH.ClassName "text-green"
        ]
      )
      <> if selected then [] else [ HH.ClassName "invisible" ]

-- Provided an array of items and any additional HTML, renders the container
-- Items should have already been treated with `boldMatches` by this point.
itemContainer
  :: ∀ p action
   . Maybe Int
  -> Array HH.PlainHTML
  -> Array (HH.HTML p (Select.Action action))
  -> HH.HTML p (Select.Action action)
itemContainer highlightIndex itemsHTML addlHTML =
  HH.div
    ( Setters.setContainerProps [ HP.classes itemContainerClasses ] )
    ( renderItems <> addlHTML )
  where
    hover :: Int -> Array HH.ClassName
    hover i = if highlightIndex == Just i then  [ HH.ClassName "bg-grey-lighter" ] else mempty

    renderItems :: Array (HH.HTML p (Select.Action action))
    renderItems =
      [ HH.ul
        [ HP.classes ulClasses ]
        $ mapWithIndex
          ( \i h ->
              HH.li
                ( Setters.setItemProps i
                  [ HP.classes $ liClasses <> hover i ]
                )
                [ HH.fromPlainHTML h ]
          )
          itemsHTML
      ]


-- Provided an array of selection items, renders them in a container
-- Make sure the array of items includes the correct click handlers
selectionContainer :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
selectionContainer []   =
  HH.div_ []
selectionContainer html =
  HH.div
  [ HP.classes selectionContainerClasses ]
  [ HH.ul
    [ HP.classes ulClasses ]
    $ html <#>
    ( \h ->
        HH.li
          [ HP.classes (HH.ClassName "py-2" : liClasses) ]
          [ h ]
    )
  ]


selectionGroup
  :: ∀ item i p
   . (item -> HH.PlainHTML)
  -> Array (HH.IProp HTMLdiv p)
  -> Array (HH.IProp HTMLbutton p)
  -> item
  -> HH.HTML i p
selectionGroup f divprops btnprops item =
  HH.div
    ([ HP.classes $ selectionGroupClasses ] <&> divprops)
    [ HH.fromPlainHTML (f item)
    , HH.button
      ([ HP.classes buttonClasses ] <&> btnprops)
      [ HH.text "✕" ]
    ]


-- Takes a key to the segment that you want to display highlighted.
-- WARN: If the key you provided does not exist in the map, your item will not be
-- rendered!
boldMatches :: ∀ item i p. String -> Fuzzy item -> Array (HH.HTML i p)
boldMatches key (Fuzzy { segments }) = boldMatch <$> (fromMaybe [ Left key ] $ lookup key segments)
  where
    boldMatch (Left str) = HH.text str
    boldMatch (Right str) = HH.span [ HP.class_ $ HH.ClassName "font-bold" ] [ HH.text str ]
