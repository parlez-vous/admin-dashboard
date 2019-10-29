{--
Horizontal Nav
--}

module UI.Hnav exposing (hnav)

import Html exposing (Html, div, header, nav)
import Html.Attributes as Attributes exposing (class)
import UI.Icons exposing (logo)

hnav : List (Html msg) -> Html msg
hnav children =
  header [ class "w-auto h-20 pt-6 pb-2 px-4 flex justify-between" ]
    [ div []
        [ logo "45"
        ]
    , nav [] children
    ]
