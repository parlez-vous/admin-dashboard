{--
Horizontal Nav
--}

module UI.Hnav exposing (hnav)

import Html exposing (Html, header, nav)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events exposing (onClick)

hnav : List (Html msg) -> Html msg
hnav children =
  header [ class "w-auto h-16 pt-4 pb-2 md:px-4 mt-3 flex justify-end" ]
    [ nav [] children ]
