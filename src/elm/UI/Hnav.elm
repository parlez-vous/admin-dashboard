{--
Horizontal Nav
--}

module UI.Hnav exposing (hnav)

import Html exposing (Html, nav, div)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events exposing (onClick)

hnav : List (Html msg) -> Html msg
hnav children =
  nav [ class "pt-4 pb-2 overflow-auto" ]
    [ div [ class "float-right" ] children ]
