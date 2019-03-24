module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (..)
import Http
import Url
import Url.Parser as Parser exposing (Parser, map, oneOf, s, top)
import List


import Api
import Api.Deserialize as Input
import SharedState exposing (SharedState)
import Routes.Admin as Admin
import Routes.Home as Home
import Routes.Router as Router
import Session


type alias Model =
  { state : AppState
  , url   : Url.Url
  }

type AppState
  = Ready SharedState Router.Model
  | NotReady

type Route
  = HomeRoute Home.Model
  | Admin Session.User
  | NotFound



type Msg
  = UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest
  | SessionVerified (Result Http.Error Input.Admin)
  | RouterMsg Router.Msg


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






-- type alias Model = Int

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    ( { state = NotReady
      , url   = url
      }
    , Api.getAdminSession SessionVerified
    )




-- UPDATE



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RouterMsg routerMsg -> ( model, Cmd.none )
    
    UrlChanged url -> ( model, Cmd.none )

    LinkClicked _ ->
      (Debug.log "link clicked" model, Cmd.none)

    SessionVerified _ -> ( model, Cmd.none )



-- VIEW

view : Model -> Browser.Document Msg
view model =
  case model.state of
    NotReady ->
      { title = "Loading"
      , body = [ div [] [ text "Loading ..." ] ]
      }

    Ready sharedState routeModel ->
      Router.view RouterMsg sharedState routeModel

