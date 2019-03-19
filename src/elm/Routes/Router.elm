module Routes.Router exposing
  ( Details
  , view
  , fromUrl
  , Route(..)
  )


import Html as Html exposing (..)
import Browser
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s, string)

type Route
  = Home
  | Admin


type alias Details msg =
  { title : String
  , header : Html msg
  , children : List (Html msg)
  }


parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home Parser.top
    , Parser.map Admin (s "admin")
    ]

fromUrl : Url -> Maybe Route
fromUrl = Parser.parse parser


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
