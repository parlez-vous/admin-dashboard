module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import RemoteData
import Url



import Api
import Api.Deserialize as Input
import SharedState exposing (SharedState)
import Router
import Session


type alias Model =
  { state  : SharedState
  , router : Router.Model
  }



type Msg
  = UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest
  | SessionVerified Input.SessionToken Nav.Key (Result Http.Error Input.Admin)
  | RouterMsg Router.Msg


main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }






type alias Flags =
  { token : Maybe Input.SessionToken
  , api   : String
  }

-- type alias Model = Int

init : Flags -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
  let
    defaultSharedState =
      SharedState.init key flags.api

    (sharedState, sessionCmd) = case flags.token of
      Just t ->
        ( defaultSharedState
        , Api.getAdminSession t flags.api <| SessionVerified t key
        )

      Nothing ->
        ( { defaultSharedState
            | session = RemoteData.Success Session.Guest
          }
        , Cmd.none
        )

    (routerModel, routerCmd) = Router.init url sharedState
  in
    ( { state  = sharedState
      , router = routerModel
      }
    , Cmd.batch
        [ Cmd.map RouterMsg routerCmd
        , sessionCmd
        ] 
    )




-- UPDATE



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RouterMsg routerMsg ->
      updateRouter routerMsg model
    
    UrlChanged url ->
      updateRouter (Router.UrlChange url) model

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model
          , Nav.pushUrl model.state.navKey <| Url.toString url
          )
          
        -- leaving the app!
        Browser.External urlStr ->
          ( model
          , Nav.load urlStr
          )

    SessionVerified token key result ->
      let
        session = case result of
          Ok admin ->
            Session.Admin (admin, token)

          _ ->
            Session.Guest

        newSharedState =
          SharedState.update (SharedState.UpdateSession session) model.state

        cmd = Router.transitionTrigger model.router newSharedState

        in
        ( { model | state = newSharedState }
        , Cmd.map RouterMsg cmd
        )



updateRouter : Router.Msg -> Model -> ( Model, Cmd Msg )
updateRouter routerMsg model =
  let
    ( nextRouterModel, routerCmd, sharedStateUpdate ) =
      Router.update model.state routerMsg model.router

    nextSharedState =
      SharedState.update sharedStateUpdate model.state

  in
  ( { model
      | state = nextSharedState,
        router = nextRouterModel
    }
  , Cmd.map RouterMsg routerCmd
  )




-- VIEW

view : Model -> Browser.Document Msg
view { state, router } =
  Router.view RouterMsg state router
