module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Url



import Api
import Api.Deserialize as Input
import SharedState exposing (SharedState)
import Router
import Session


type alias Model =
  { state : AppState
  , url   : Url.Url
  }


{--
TODO: refactor
https://github.com/parlez-vous/site/issues/4

Ready means one of two things:
 - We have a token stored locally AND we have _tried_ to
   get the user's info with the given token

 - There is no stoken stored locally. Therefore we label
   the user as a guest


NotReady means one thing:
 - There is a token stored locally AND we have NOT yet
   tried to get the user's info with the given token
--}
type AppState
  = Ready SharedState Router.Model
  | NotReady Nav.Key Input.SessionToken String


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
    (state, cmd) = case flags.token of
      Just t  ->
        ( NotReady key t flags.api
        , Api.getAdminSession t flags.api <| SessionVerified t key
        )

      Nothing ->
        let
          ( routerModel, routerCmd ) = Router.init flags.api url key Session.Guest

        in
        ( Ready
            (SharedState.init key Session.Guest flags.api)
            routerModel
        , Cmd.map RouterMsg routerCmd
        )

  in
  ( { state = state
    , url   = url
    }
  , cmd
  )




-- UPDATE



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RouterMsg routerMsg ->
      updateRouter routerMsg model
    
    UrlChanged url ->
      updateRouter (Router.UrlChange url) { model | url = url }

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          let
            navKey = case model.state of
              Ready sharedState _ ->
                sharedState.navKey

              NotReady navKey_ _ _ ->
                navKey_

          in
          ( model
          , Nav.pushUrl navKey <| Url.toString url
          )
          
        -- leaving the app!
        Browser.External urlStr ->
          ( model
          , Nav.load urlStr
          )

    SessionVerified token key result ->
      let
        api =
          case model.state of
            Ready sharedState _ -> sharedState.api
            NotReady _ _ api_    -> api_

      in
      case result of
        Ok admin ->
          let
            adminSession = Session.Admin (admin, token)

            ( routerModel, routerCmd ) = Router.init api model.url key adminSession
          
            sharedState =
              SharedState.init key adminSession api

          in
          ( { model
              | state = Ready sharedState routerModel
            }
          , Cmd.map RouterMsg routerCmd
          )

        Err e ->
          let
            _ = (Debug.log "Error while verifying session" e)

            ( routerModel, routerCmd ) = Router.init api model.url key Session.Guest

            sharedState =
              SharedState.init key Session.Guest api

          in
            ( { model
                | state = Ready sharedState routerModel
              }
            , Cmd.map RouterMsg routerCmd
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
    Ready sharedState routeModel ->
      Router.view RouterMsg sharedState routeModel

    _ ->
      { title = "Loading"
      , body = [ div [] [ text "Loading ..." ] ]
      }


