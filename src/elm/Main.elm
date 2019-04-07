module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Url
import List


import Api
import Api.Deserialize as Input
import SharedState exposing (SharedState, updateSession)
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
  | NotReady Nav.Key



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
    ( { state = NotReady key
      , url   = url
      }
    , Api.getAdminSession SessionVerified
    )




-- UPDATE



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    _ = Debug.log "UPDATE" msg
    
  in
  case msg of
    RouterMsg routerMsg ->
      updateRouter routerMsg model
    
    UrlChanged url ->
      updateRouter (Router.UrlChange url) { model | url = url }

    LinkClicked _ ->
      (Debug.log "link clicked" model, Cmd.none)

    SessionVerified result ->
      case result of
        Ok admin ->
          case model.state of
            Ready sharedState routerModel ->
              ( { model
                  | state = Ready (SharedState.updateSession (Session.Admin (admin, "placeholder")) sharedState) routerModel
                }
              , Cmd.none
              )

            NotReady key ->
              ( { model
                  | state = Ready (SharedState.init key <| Session.Admin (admin, "placeholder")) (Router.init model.url)
                }
              , Cmd.none
              )

        Err e ->
          let
            _ = (Debug.log "Error while verifying session" e)

          in
            case model.state of
              Ready sharedState routerModel ->
                ( { model
                    | state = Ready (SharedState.updateSession Session.Guest sharedState) routerModel
                  }
                , Cmd.none
                )

              NotReady key ->
                ( { model
                    | state = Ready (SharedState.init key Session.Guest) (Router.init model.url)
                  }
                , Cmd.none
                )



updateRouter : Router.Msg -> Model -> ( Model, Cmd Msg )
updateRouter routerMsg model =
    case model.state of
        Ready sharedState routerModel ->
            let
                ( nextRouterModel, routerCmd, sharedStateUpdate ) =
                    Router.update sharedState routerMsg routerModel

                nextSharedState =
                    SharedState.update sharedStateUpdate sharedState

            in
            ( { model | state = Ready nextSharedState nextRouterModel }
            , Cmd.map RouterMsg routerCmd
            )

        _ ->
            let
                _ =
                    Debug.log "We got a router message even though the app is not ready?"
                        routerMsg
            in
            ( model, Cmd.none )



-- VIEW

view : Model -> Browser.Document Msg
view model =
  case model.state of
    NotReady _ ->
      { title = "Loading"
      , body = [ div [] [ text "Loading ..." ] ]
      }

    Ready sharedState routeModel ->
      let
        _ = Debug.log "SHARED STATE" sharedState
      in  
      Router.view RouterMsg sharedState routeModel

