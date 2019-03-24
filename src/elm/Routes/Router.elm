module Routes.Router exposing
  ( fromUrl
  , Route(..)
  , Model
  , Msg
  , view
  )


import Html as Html exposing (..)
import Browser
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s, string)



import Routes.Home as Home
import Routes.Admin as Admin 
import SharedState exposing (SharedState)



{-
Once the application gets more complex,
you can import the models from various routes

import Routes.Home as Home
import Routes.Settings as Settings

type alias Model =
  { home     : Home.Model
  , settings : Settings.Model
  , .....
  , route    : Route
  }
-}
type alias Model =
  { homeModel : Home.Model
  , route     : Route
  }

type Route
  = Home
  | Admin
  | NotFound


type Msg
  = HomeMsg Home.Msg
  | AdminMsg Admin.Msg


parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home Parser.top
    , Parser.map Admin (s "admin")
    ]

fromUrl : Url -> Route
fromUrl = Maybe.withDefault NotFound << Parser.parse parser




view : (Msg -> msg) -> SharedState -> Model -> Browser.Document msg
view toMsg sharedState routerModel =
  let
    ( title, html ) =
      case routerModel.route of
        Home ->
          Home.view routerModel.homeModel
          |> Tuple.mapSecond (Html.map HomeMsg)
          |> Tuple.mapSecond (Html.map toMsg)
          

        Admin ->
          Admin.view
          |> Tuple.mapSecond (Html.map AdminMsg)
          |> Tuple.mapSecond (Html.map toMsg)

        NotFound ->
          ( "Woops!", div [] [ text "404 Not Found"] )

  in
  { title = title ++ " | Parlez-Vous "
  , body = [ html ]
  }