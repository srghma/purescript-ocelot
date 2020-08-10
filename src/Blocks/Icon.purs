module Ocelot.Block.Icon where

import Prelude hiding (add)

import DOM.HTML.Indexed (HTMLspan)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Ocelot.HTML.Properties ((<&>))

icon
  :: ∀ p i
   . HH.ClassName
  -> Array (HH.IProp HTMLspan i)
  -> HH.HTML p i
icon (HH.ClassName className) iprops =
  HH.span
    [ HA.label className
    , HP.classes $
      [ HH.ClassName "inline-block"
      ]
    ]
    [ HH.span
        ( iprops <&>
          [ HA.hidden "true"
          , HP.classes $
            [ HH.ClassName className
            , HH.ClassName "inline-block"
            ]
          ]
        )
        []
    ]

add :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
add = icon (HH.ClassName "icon-add")

add_ :: ∀ p i. HH.HTML p i
add_ = add []

added :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
added = icon (HH.ClassName "icon-added")

added_ :: ∀ p i. HH.HTML p i
added_ = added []

arrowDown :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowDown = icon (HH.ClassName "icon-arrow-down")

arrowDown_ :: ∀ p i. HH.HTML p i
arrowDown_ = arrowDown []

arrowLeft :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowLeft = icon (HH.ClassName "icon-arrow-left")

arrowLeft_ :: ∀ p i. HH.HTML p i
arrowLeft_ = arrowLeft []

arrowRight :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowRight = icon (HH.ClassName "icon-arrow-right")

arrowRight_ :: ∀ p i. HH.HTML p i
arrowRight_ = arrowRight []

arrowUp :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowUp = icon (HH.ClassName "icon-arrow-up")

arrowUp_ :: ∀ p i. HH.HTML p i
arrowUp_ = arrowUp []

back :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
back = icon (HH.ClassName "icon-back")

back_ :: ∀ p i. HH.HTML p i
back_ = back []

caratDown :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratDown = icon (HH.ClassName "icon-carat-down")

caratDown_ :: ∀ p i. HH.HTML p i
caratDown_ = caratDown []

caratLeft :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratLeft = icon (HH.ClassName "icon-carat-left")

caratLeft_ :: ∀ p i. HH.HTML p i
caratLeft_ = caratLeft []

caratRight :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratRight = icon (HH.ClassName "icon-carat-right")

caratRight_ :: ∀ p i. HH.HTML p i
caratRight_ = caratRight []

caratUp :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratUp = icon (HH.ClassName "icon-carat-up")

caratUp_ :: ∀ p i. HH.HTML p i
caratUp_ = caratUp []

chevronLeft :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
chevronLeft = icon (HH.ClassName "icon-chevron-left")

chevronLeft_ :: ∀ p i. HH.HTML p i
chevronLeft_ = chevronLeft []

chevronRight :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
chevronRight = icon (HH.ClassName "icon-chevron-right")

chevronRight_ :: ∀ p i. HH.HTML p i
chevronRight_ = chevronRight []

close :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
close = icon (HH.ClassName "icon-close")

close_ :: ∀ p i. HH.HTML p i
close_ = close []

collapse :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
collapse = icon (HH.ClassName "icon-collapse")

collapse_ :: ∀ p i. HH.HTML p i
collapse_ = collapse []

dataSources :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
dataSources = icon (HH.ClassName "icon-data-sources")

dataSources_ :: ∀ p i. HH.HTML p i
dataSources_ = dataSources []

delete :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
delete = icon (HH.ClassName "icon-delete")

delete_ :: ∀ p i. HH.HTML p i
delete_ = delete []

deleteCircle :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
deleteCircle = icon (HH.ClassName "icon-delete-circle")

deleteCircle_ :: ∀ p i. HH.HTML p i
deleteCircle_ = deleteCircle []

download :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
download = icon (HH.ClassName "icon-download")

download_ :: ∀ p i. HH.HTML p i
download_ = download []

error :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
error = icon (HH.ClassName "icon-error")

error_ :: ∀ p i. HH.HTML p i
error_ = error []

expand :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
expand = icon (HH.ClassName "icon-expand")

expand_ :: ∀ p i. HH.HTML p i
expand_ = expand []

facebook :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
facebook = icon (HH.ClassName "icon-facebook")

facebook_ :: ∀ p i. HH.HTML p i
facebook_ = facebook []

info :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
info = icon (HH.ClassName "icon-info")

info_ :: ∀ p i. HH.HTML p i
info_ = info []

instagram :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
instagram = icon (HH.ClassName "icon-instagram")

instagram_ :: ∀ p i. HH.HTML p i
instagram_ = instagram []

menu :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
menu = icon (HH.ClassName "icon-menu")

menu_ :: ∀ p i. HH.HTML p i
menu_ = menu []

navigate :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
navigate = icon (HH.ClassName "icon-navigate")

navigate_ :: ∀ p i. HH.HTML p i
navigate_ = navigate []

options :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
options = icon (HH.ClassName "icon-options")

options_ :: ∀ p i. HH.HTML p i
options_ = options []

plus :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
plus = icon (HH.ClassName "icon-plus")

plus_ :: ∀ p i. HH.HTML p i
plus_ = plus []

refresh :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
refresh = icon (HH.ClassName "icon-refresh")

refresh_ :: ∀ p i. HH.HTML p i
refresh_ = refresh []

search :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
search = icon (HH.ClassName "icon-search")

search_ :: ∀ p i. HH.HTML p i
search_ = search []

selected :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
selected = icon (HH.ClassName "icon-selected")

selected_ :: ∀ p i. HH.HTML p i
selected_ = selected []

settings :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
settings = icon (HH.ClassName "icon-settings")

settings_ :: ∀ p i. HH.HTML p i
settings_ = settings []

share :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
share = icon (HH.ClassName "icon-share")

share_ :: ∀ p i. HH.HTML p i
share_ = share []

success :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
success = icon (HH.ClassName "icon-success")

success_ :: ∀ p i. HH.HTML p i
success_ = success []

timeline :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
timeline = icon (HH.ClassName "icon-timeline")

timeline_ :: ∀ p i. HH.HTML p i
timeline_ = timeline []

tip :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
tip = icon (HH.ClassName "icon-tip")

tip_ :: ∀ p i. HH.HTML p i
tip_ = tip []

twitter :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
twitter = icon (HH.ClassName "icon-twitter")

twitter_ :: ∀ p i. HH.HTML p i
twitter_ = twitter []

