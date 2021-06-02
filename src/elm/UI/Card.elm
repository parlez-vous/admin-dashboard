module UI.Card exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


card : List (Html msg) -> Html msg
card body =
    div [class "rounded"] body
