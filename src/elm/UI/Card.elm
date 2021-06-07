module UI.Card exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


card : String -> List (Html msg) -> Html msg
card classes body =
    div [ class ("max-w-md py-4 px-8 bg-white shadow-md rounded-sm my-10 mt-2 " ++ classes) ] body
