module Routes.Router exposing
  ( Model
  , Msg(..)
  , init
  , update
  , view
  )


import Html as Html exposing (..)
import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s, string)



import Routes.Home as Home
import Routes.Admin as Admin 
import Session
import SharedState exposing (SharedState, SharedStateUpdate)



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
  { homeModel  : Home.Model
  , adminModel : Admin.Model
  , route      : Route
  }

type Route
  = Home
  | Admin
  | NotFound


type Msg
  = UrlChange Url
  | HomeMsg Home.Msg
  | AdminMsg Admin.Msg


parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home Parser.top
    , Parser.map Admin (s "admin")
    ]

fromUrl : Url -> Route
fromUrl = Maybe.withDefault NotFound << Parser.parse parser


init : Url -> Nav.Key -> Session.User -> ( Model, Cmd msg )
init url navKey session =
  let
    route = fromUrl url

    cmd =
      case ( session, route ) of
        -- If guest visits a private route, redirect them to the home page
        ( Session.Guest, Admin ) -> Nav.pushUrl navKey "/"

        _ -> Cmd.none

  in
  ( { homeModel = Home.init
    , adminModel = Admin.init
    , route     = route
    }
  , cmd
  )



update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case msg of
    UrlChange url -> 
      ( { model | route = fromUrl url }
      , Cmd.none
      , SharedState.NoUpdate
      )
      
    HomeMsg homeMsg ->
      let 
        ( homeModel, homeCmd, sharedStateUpdate ) =
          Home.update state homeMsg model.homeModel
      in
        ( { model
            | homeModel = homeModel
          }
        , Cmd.map HomeMsg homeCmd
        , sharedStateUpdate
        )

    AdminMsg adminMsg ->
      let
        ( adminModel, adminCmd, sharedStateUpdate ) =
          Admin.update state adminMsg model.adminModel
      in
      ( { model
          | adminModel = adminModel
        }
      , Cmd.map AdminMsg adminCmd
      , sharedStateUpdate
      )



view : (Msg -> msg) -> SharedState -> Model -> Browser.Document msg
view toMsg sharedState routerModel =
  let
    ( title, html ) =
      case routerModel.route of
        Home ->
          Home.view sharedState.session routerModel.homeModel
          |> Tuple.mapSecond (Html.map HomeMsg)
          |> Tuple.mapSecond (Html.map toMsg)
          

        Admin ->
          case sharedState.session of
            Session.Guest ->
              ( "Redirecting ..."
              , div [] [ text "Redirecting ..."]
              )
            
            Session.Admin ( admin, _ ) -> 
              Admin.view sharedState admin routerModel.adminModel
              |> Tuple.mapSecond (Html.map AdminMsg)
              |> Tuple.mapSecond (Html.map toMsg)

        NotFound ->
          ( "Woops!", div [] [ text "404 Not Found"] )

  in
  { title = title ++ " | Parlez-Vous "
  , body = [ html ]
  }