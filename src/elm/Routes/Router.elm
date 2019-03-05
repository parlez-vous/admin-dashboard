module Routes.Router exposing
  ( Details
  , view
  )


import Html as Html exposing (..)
import Browser


type alias Details msg =
  { title : String
  , header : Html msg
  , children : List (Html msg)
  }


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
  { title =
      details.title
  , body =
      [ Html.map toMsg <| details.header
      , Html.map toMsg <|
          (div [] details.children)
      ]
  }
