module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import RemoteData exposing (WebData)
import Url



import Api
import Api.Deserialize as Input
import SharedState exposing (SharedState)
import Router
import Session


type alias NotReadyData =
  { navKey  : Nav.Key
  , api     : String
  , session : WebData Session.User
  }


type alias AppData = 
  { state  : SharedState
  , router : Router.Model
  }

type Model
  = Ready AppData
  | NotReady NotReadyData



type Msg
  = UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest
  | SessionVerified Input.SessionToken Url.Url (Result Http.Error Input.Admin)
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
    (model, cmd) = case flags.token of
      Just t ->
        ( NotReady (NotReadyData key flags.api RemoteData.NotAsked)
        , Api.getAdminSession t flags.api <| SessionVerified t url
        )
        
      Nothing ->
        let
          sharedState = SharedState key flags.api Session.Guest RemoteData.NotAsked
          ( routerModel, routerCmd ) = Router.init url sharedState

          appData =
            { state = sharedState
            , router = routerModel 
            }
          
        in
        ( Ready appData
        , Cmd.map RouterMsg routerCmd
        )

  in
    ( model
    , cmd
    )




-- UPDATE

getNavKey : Model -> Nav.Key
getNavKey model =
  case model of
    Ready { state } -> state.navKey
    NotReady { navKey } -> navKey


getApi : Model -> String
getApi model =
  case model of
    Ready { state } -> state.api
    NotReady { api } -> api


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
          , Nav.pushUrl (getNavKey model) <| Url.toString url
          )
          
        -- leaving the app!
        Browser.External urlStr ->
          ( model
          , Nav.load urlStr
          )

    SessionVerified token url result ->
      let
        session = case result of
          Ok admin ->
            Session.Admin (admin, token)

          _ ->
            Session.Guest

        key = getNavKey model
        api = getApi model

        sharedState =
          SharedState.init key api session

        (routerModel, routerCmd) = Router.init url sharedState

        in
        ( Ready
          { state = sharedState,
            router = routerModel 
          }
        , Cmd.map RouterMsg routerCmd
        )



updateRouter : Router.Msg -> Model -> ( Model, Cmd Msg )
updateRouter routerMsg model =
  case model of
    NotReady _ -> (model, Cmd.none)

    Ready appData ->
      let
        ( nextRouterModel, routerCmd, sharedStateUpdate ) =
          Router.update appData.state routerMsg appData.router

        nextSharedState =
          SharedState.update sharedStateUpdate appData.state

      in
      ( Ready { appData
          | state = nextSharedState,
            router = nextRouterModel
        }
      , Cmd.map RouterMsg routerCmd
      )




-- VIEW

view : Model -> Browser.Document Msg
view model =
  case model of
    Ready { state, router } ->
      Router.view RouterMsg state router

    NotReady _ ->
      { title = "Loading ..."
      , body = [div [] [ text "Loading ..." ]]
      }
      
