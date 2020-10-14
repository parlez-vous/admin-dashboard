module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Url



import Api
import Api.Deserialize as Input
import SharedState exposing (SharedState(..))
import Router


type alias NotReadyData =
  { navKey  : Nav.Key
  , api     : String
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
        ( NotReady (NotReadyData key flags.api)
        , Api.getAdminSession t flags.api <| SessionVerified t url
        )
        
      Nothing ->
        let
          sharedState = Public <| SharedState.init key flags.api 

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
    Ready { state } ->
      case state of
        Public { navKey } -> navKey
        Private { navKey } -> navKey
        
    NotReady { navKey } -> navKey


getApi : Model -> String
getApi model =
  case model of
    Ready { state } ->
      case state of
        Public { api } -> api
        Private { api } -> api

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
        publicState = SharedState.init key api

        sharedState = case result of
          Ok admin ->
            Private
            <| SharedState.toPrivate (admin, token) publicState

          _ ->
            Public publicState

        key = getNavKey model
        api = getApi model

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
        let
            { title, body } = Router.view state router
        in
        { title = title
        , body = [ Html.map RouterMsg body ]
        }

    NotReady _ ->
      { title = "Loading ..."
      , body = [div [] [ text "Loading ..." ]]
      }
      
