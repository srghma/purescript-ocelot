-- ATTRIBUTION:
-- This module adapts from rnons' Halogen Storybook
-- https://github.com/rnons/purescript-halogen-storybook/
-- to fit with our UI guide branding needs
module UIGuide.App
  ( Stories
  , StoryQuery
  , Page(..)
  , Group(..)
  , runStorybook
  , module Halogen.Storybook.Proxy
  ) where

import Prelude
import TailwindClasses as TailwindClasses
import Data.Const (Const)
import Data.Functor (mapFlipped)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, launchAff_)
import Global.Unsafe (unsafeDecodeURI, unsafeEncodeURI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Storybook.Proxy (proxy)
import Halogen.VDom.Driver (runUI)
import Ocelot.Block.Format as Format
import Routing.Hash (hashes)
import UIGuide.Block.Backdrop as Backdrop
import Web.HTML.HTMLElement (HTMLElement)

data Query a
  = RouteChange String a

type Action
  = Unit

type State m
  = { route :: String
    , stories :: Stories m
    , partitions :: M.Map Group (Stories m)
    }

type StoryQuery
  = Const Void

type Stories m
  = M.Map String (Page m)

type Page m
  = { anchor :: String
    , component :: H.Component HH.HTML StoryQuery Unit Void m
    , group :: Group
    }

data Group
  = Basics
  | FormElements
  | Components

derive instance eqGroup :: Eq Group

derive instance ordGroup :: Ord Group

instance showGroup :: Show Group where
  show Basics = "Basics"
  show FormElements = "Form Elements"
  show Components = "Components"

type HTML m
  = H.ComponentHTML Action Slots m

type Slots
  = ( child :: H.Slot StoryQuery Void String )

_child = SProxy :: SProxy "child"

-- | Takes stories config and mount element, and renders the storybook.
runStorybook ::
  Stories Aff ->
  Array Group ->
  HTMLElement ->
  Aff Unit
runStorybook stories groups body = do
  app' <- runUI app { stories, groups } body
  void $ H.liftEffect $ hashes
    $ \_ next ->
        launchAff_ $ app'.query (H.tell $ RouteChange $ unsafeDecodeURI next)

type Input m
  = { stories :: Stories m
    , groups :: Array Group
    }

app :: ∀ m. H.Component HH.HTML Query (Input m) Void m
app =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = eval }
    }
  where
  initialState :: Input m -> State m
  initialState i = { route: "", stories: i.stories, partitions: M.fromFoldable $ flip partitionByGroup i.stories <$> i.groups }

  render :: State m -> HTML m
  render state =
    HH.body_
      [ HH.div
          [ HP.classes [ HH.ClassName "min-h-screen" ] ]
          [ renderSidebar state
          , renderContainer state
          ]
      ]

  renderContainer :: State m -> HTML m
  renderContainer state =
    HH.div
      [ HP.classes [ HH.ClassName "md:ml-80" ] ]
      [ HH.div
          [ HP.classes
            [ TailwindClasses.fixed
            , TailwindClasses.w_full
            ]
          ]
          [ HH.div
              [ HP.classes [ TailwindClasses.top_0, TailwindClasses.bg_white, HH.ClassName "md:hidden", TailwindClasses.relative, TailwindClasses.border_b, HH.ClassName "border-gray-light", TailwindClasses.h_12, TailwindClasses.py_8, TailwindClasses.flex, TailwindClasses.items_center ] ]
              [ HH.a
                  [ HP.classes [ TailwindClasses.mx_auto, TailwindClasses.inline_flex, TailwindClasses.items_center ]
                  , HP.href ""
                  ]
                  [ HH.text "CitizenNet UI Guide" ]
              ]
          ]
      , HH.div
          [ HP.classes [ TailwindClasses.p_12, TailwindClasses.w_full, TailwindClasses.container, TailwindClasses.mx_auto ] ]
          [ renderSlot state ]
      ]

  renderSlot :: State m -> HTML m
  renderSlot state = case M.lookup state.route state.stories of
    Just { component } -> HH.slot _child state.route component unit absurd
    -- TODO: Fill in a home page HTML renderer
    _ -> HH.div_ []

  renderSidebar :: State m -> HTML m
  renderSidebar state =
    Backdrop.backdrop
      [ HP.id_ "sidebar"
      , HP.classes
          [ TailwindClasses.hidden
          , TailwindClasses.fixed
          , HH.ClassName "inset-y-0"
          , TailwindClasses.left_0
          , HH.ClassName "overflow-y-auto"
          , HH.ClassName "md:overflow-visible"
          , TailwindClasses.scrolling_touch
          , HH.ClassName "md:scrolling-auto"
          , HH.ClassName "w-4/5"
          , HH.ClassName "md:w-full"
          , HH.ClassName "md:max-w-xs"
          , TailwindClasses.flex_none
          -- , HH.ClassName "border-r-2"
          -- , HH.ClassName "border-gray-light"
          , HH.ClassName "md:flex"
          , TailwindClasses.flex_col
          ]
      ]
      [ HH.div
          [ HP.classes [ TailwindClasses.flex_1, TailwindClasses.p_6, HH.ClassName "overflow-y-auto" ] ]
          [ HH.header_
              [ Format.heading
                  [ HP.class_ $ TailwindClasses.flex ]
                  [ HH.img
                      [ HP.classes [ TailwindClasses.mr_2 ]
                      , HP.src "https://citizennet.com/manager/images/logo.svg"
                      ]
                  , HH.text "Ocelot"
                  ]
              ]
          , HH.nav
              [ HP.classes [ TailwindClasses.text_base, HH.ClassName "overflow-y-auto" ] ]
              (renderGroups state)
          ]
      ]

  renderGroups :: State m -> Array (HTML m)
  renderGroups state =
    mapFlipped (M.toUnfoldable state.partitions)
      $ \(Tuple group stories) ->
          HH.div
            [ HP.classes [ TailwindClasses.mb_6 ] ]
            [ Format.caption_
                [ HH.text $ show group ]
            , renderGroup state.route stories
            ]

  renderGroup :: String -> Stories m -> HTML m
  renderGroup route stories =
    HH.ul []
      $ mapFlipped (M.toUnfoldable stories)
      $ \(Tuple href { anchor }) ->
          HH.li
            [ HP.classes [ TailwindClasses.mb_3 ] ]
            [ HH.a
                [ HP.classes
                    $ Format.linkClasses
                    <> (if href == route then [ TailwindClasses.font_medium ] else [])
                , HP.href $ "#" <> unsafeEncodeURI href
                ]
                [ HH.text anchor ]
            ]

  eval :: forall a. Query a -> H.HalogenM (State m) Action Slots Void m (Maybe a)
  eval (RouteChange route next) = do
    H.modify_ (\state -> state { route = route })
    pure $ Just next

----------
-- Helpers
partitionByGroup :: ∀ m. Group -> Stories m -> Tuple Group (Stories m)
partitionByGroup g = Tuple g <<< M.filter (\{ group } -> group == g)
