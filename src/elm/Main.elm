module Main exposing (Model, main)

import Ant.Css
import Api exposing (Api)
import Api.Deserialize as Input
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Router
import SharedState exposing (SharedState(..))
import Task
import Time
import Url


type alias NotReadyData =
    { navKey : Nav.Key
    , api : Api
    }


type alias AppData =
    { state : SharedState
    , router : Router.Model
    }


type FailureCode
    = E_101



-- | E_102
-- | E_103
-- | etc ...


type alias FailureCodes =
    { flagApiIsInvalidUrl : FailureCode
    }


failureCodes : FailureCodes
failureCodes =
    { flagApiIsInvalidUrl = E_101
    }


failureCodeToString : FailureCode -> String
failureCodeToString _ =
    "e-101"


type AppState
    = Ready AppData
      -- represents a pending state for the application
      -- such as when we're checking with the server if a session token is valid
    | NotReady NotReadyData
      -- initialization failed
    | FailedInit FailureCode Nav.Key


type alias Model =
    { appState : AppState
    , timeZone : Time.Zone
    }


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | SessionVerified Input.SessionToken Url.Url Api (Result Http.Error Input.Admin)
    | GotUserTimeZone Time.Zone
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
    , api : String
    }



-- type alias Model = Int


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags browserUrl key =
    let
        maybeApiUrl =
            Url.fromString flags.api

        ( initModel, cmd ) =
            case ( maybeApiUrl, flags.token ) of
                -- The incoming Url from flags must be a valid URL, otherwise we can't make any API Requests
                ( Nothing, _ ) ->
                    ( FailedInit failureCodes.flagApiIsInvalidUrl key, Cmd.none )

                ( Just apiUrl, Just token ) ->
                    let
                        api =
                            Api.apiFactory apiUrl

                        { getAdminSession } =
                            Api.getApiClient api
                    in
                    ( NotReady <| NotReadyData key api
                    , getAdminSession token <| SessionVerified token browserUrl api
                    )

                ( Just apiUrl, Nothing ) ->
                    let
                        api =
                            Api.apiFactory apiUrl

                        sharedState =
                            Public <| SharedState.init key api

                        ( routerModel, routerCmd ) =
                            Router.init browserUrl sharedState

                        appData =
                            { state = sharedState
                            , router = routerModel
                            }
                    in
                    ( Ready appData
                    , Cmd.map RouterMsg routerCmd
                    )
    in
    ( { appState = initModel
      , timeZone = Time.utc
      }
    , Cmd.batch
        [ cmd
        , Time.here |> Task.perform GotUserTimeZone
        ]
    )



-- UPDATE


getNavKey : Model -> Nav.Key
getNavKey { appState } =
    case appState of
        Ready { state } ->
            case state of
                Public { navKey } ->
                    navKey

                Private { navKey } ->
                    navKey

        NotReady { navKey } ->
            navKey

        FailedInit _ navKey ->
            navKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouterMsg routerMsg ->
            updateRouter routerMsg model

        UrlChanged url ->
            updateRouter (Router.UrlChange url) model

        LinkClicked urlRequest ->
            let
                _ =
                    Debug.log "url request: " urlRequest
            in
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

        SessionVerified token browserUrl api result ->
            let
                publicState =
                    SharedState.init key api

                sharedState =
                    case result of
                        Ok admin ->
                            Private <|
                                SharedState.toPrivate ( admin, token ) publicState

                        _ ->
                            Public publicState

                key =
                    getNavKey model

                ( routerModel, routerCmd ) =
                    Router.init browserUrl sharedState
            in
            ( { model
                | appState =
                    Ready
                        { state = sharedState
                        , router = routerModel
                        }
              }
            , Cmd.map RouterMsg routerCmd
            )

        GotUserTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.none )


updateRouter : Router.Msg -> Model -> ( Model, Cmd Msg )
updateRouter routerMsg ({ appState } as model) =
    case appState of
        Ready appData ->
            let
                ( nextRouterModel, routerCmd, sharedStateUpdate ) =
                    Router.update appData.state routerMsg appData.router

                nextSharedState =
                    SharedState.update sharedStateUpdate appData.state
            in
            ( { model
                | appState =
                    Ready
                        { appData
                            | state = nextSharedState
                            , router = nextRouterModel
                        }
              }
            , Cmd.map RouterMsg routerCmd
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view { appState, timeZone } =
    case appState of
        Ready { state, router } ->
            let
                { title, body } =
                    Router.view ( { timezone = timeZone }, state ) router
            in
            { title = title
            , body = [ Ant.Css.defaultStyles, Html.map RouterMsg body ]
            }

        NotReady _ ->
            { title = "Loading ..."
            , body = [ div [] [ text "Loading ..." ] ]
            }

        FailedInit failureCode _ ->
            { title = "woops!"
            , body =
                [ div []
                    [ text "Something went wrong :("
                    , br [] []
                    , text <| "Error code: " ++ failureCodeToString failureCode
                    ]
                ]
            }
