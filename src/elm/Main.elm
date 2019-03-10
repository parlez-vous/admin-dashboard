module Main exposing (main, Msg)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (..)
import Http
import Url
import Url.Parser as Parser exposing (Parser, map, oneOf, s, top)
import List


import Routes.Admin as Admin
import Routes.Home as Home
import Routes.Router as Router


type alias Model =
  { key   : Nav.Key
  , route : Route
  }

type Route
  = HomeRoute Home.Model
  | Admin Admin.Model
  | NotFound



type Msg
  = HomeMsg Home.Msg
  | AdminMsg Admin.Msg
  | UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest

main =
  Browser.application
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- MODEL





-- type alias Model = Int

init : () -> Url.Url -> Nav.Key -> (Model, Cmd msg)
init flags url key =
  let
    -- placeholder for now
    initialRoute = HomeRoute <| Home.Model key Home.FormHidden

  in
    (Model key initialRoute, Cmd.none)



-- ROUTER

getRoute : Url.Url -> Model -> (Model, Cmd Msg)
getRoute url model =
  let
    parser =
      oneOf
        [ route top
            ( Model model.key (HomeRoute <| Home.Model model.key Home.FormHidden)
            )
        , route (s "admin")
            ( Model model.key (Admin Admin.Hello ))
        ]

  in
  case Parser.parse parser url of
    Just answer ->
      (answer, Cmd.none)

    Nothing ->
      ( { model | route = NotFound }
      , Cmd.none
      )



route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
  Parser.map handler parser






-- UPDATE


createUpdater : Nav.Key -> (m -> Route) -> (msg -> Msg) -> ((m, Cmd msg) -> (Model, Cmd Msg))
createUpdater key toRoute toMsg =
  let
    toModel = (\m -> Model key (toRoute m))

  in
    Tuple.mapBoth toModel (Cmd.map toMsg)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    updater = createUpdater model.key

  in
    case msg of
      HomeMsg homeMsg ->
        case model.route of
          HomeRoute formModel -> updater HomeRoute HomeMsg <| Home.update homeMsg formModel

          _ -> (model, Cmd.none)

      AdminMsg adminMsg -> (Debug.log "adminmsg!" model, Cmd.none)

      UrlChanged url -> getRoute url model

      LinkClicked _ ->
        (Debug.log "link clicked" model, Cmd.none)




-- VIEW

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

view : Model -> Browser.Document Msg
view model =
  case model.route of
    NotFound ->
      { title = "Woops!"
      , body = [ div [] [ text "404 not found" ] ]
      }

    HomeRoute homeModel -> Router.view HomeMsg (Home.view homeModel)

    Admin adminModel -> Router.view AdminMsg (Admin.view adminModel)
