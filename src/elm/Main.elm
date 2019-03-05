module Main exposing (main, Msg)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (..)
import Http
import Url
import List


import Routes.Home as Home
import Routes.Router as Router


type alias Model =
  { key   : Nav.Key
  , route : Route
  }

type Route
  = Home Home.Model
  | NotFound



type Msg
  = HomeMsg Home.Msg
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
    getRoute = Home Home.FormHidden

  in
    (Model key getRoute, Cmd.none)


-- UPDATE




createReducer : Nav.Key -> (m -> Route) -> (msg -> Msg) -> ((m, Cmd msg) -> (Model, Cmd Msg))
createReducer key toRoute toMsg =
  let
    toModel = (\m -> Model key (toRoute m))

  in
    Tuple.mapBoth toModel (Cmd.map toMsg)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    reducer = createReducer model.key
  
  in
    case msg of
      HomeMsg homeMsg ->
        case model.route of
          Home formModel -> reducer Home HomeMsg <| Home.update homeMsg formModel

          _ -> (model, Cmd.none)

      UrlChanged _ ->
        (Debug.log "url changed" model, Cmd.none)

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

    Home homeModel -> Router.view HomeMsg (Home.view homeModel)
