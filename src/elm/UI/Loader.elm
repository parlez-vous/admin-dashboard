-- Exposes a loding UI component


module UI.Loader exposing (donut)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


donut : Html msg
donut =
    div [ class "loading__donut" ] []
