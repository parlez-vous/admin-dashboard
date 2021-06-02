module UI.Card exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


card : List (Html msg) -> Html msg
card body =
    div [ class "max-w-md py-4 px-8 bg-white shadow-lg rounded-lg my-10 mt-2 text-gray-600" ] body
