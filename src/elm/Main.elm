module Main exposing (main)

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


type Model
    = Ready AppData
      -- represents a pending state for the application
      -- such as when we're checking with the server if a session token is valid
    | NotReady NotReadyData
      -- initialization failed
    | FailedInit FailureCode Nav.Key


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
    , api : String
    }



-- type alias Model = Int


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        maybeApiUrl =
            Url.fromString flags.api
    in
    case ( maybeApiUrl, flags.token ) of
        -- The incoming Url from flags must be a valid URL, otherwise we can't make any API Requests
        ( Nothing, _ ) ->
            ( FailedInit failureCodes.flagApiIsInvalidUrl key, Cmd.none )

        ( Just apiUrl, Just token ) ->
            let
                { getAdminSession } =
                    Api.getApiClient (Api.apiFactory apiUrl)
            in
            ( NotReady <| NotReadyData key (Api.apiFactory apiUrl)
            , getAdminSession token <| SessionVerified token url
            )

        ( Just apiUrl, Nothing ) ->
            let
                sharedState =
                    Public <| SharedState.init key apiUrl

                ( routerModel, routerCmd ) =
                    Router.init url sharedState

                appData =
                    { state = sharedState
                    , router = routerModel
                    }
            in
            ( Ready appData
            , Cmd.map RouterMsg routerCmd
            )



-- UPDATE


getNavKey : Model -> Nav.Key
getNavKey model =
    case model of
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

        SessionVerified token url result ->
            let
                publicState =
                    SharedState.init key url

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
                    Router.init url sharedState
            in
            ( Ready
                { state = sharedState
                , router = routerModel
                }
            , Cmd.map RouterMsg routerCmd
            )


updateRouter : Router.Msg -> Model -> ( Model, Cmd Msg )
updateRouter routerMsg model =
    case model of
        Ready appData ->
            let
                ( nextRouterModel, routerCmd, sharedStateUpdate ) =
                    Router.update appData.state routerMsg appData.router

                nextSharedState =
                    SharedState.update sharedStateUpdate appData.state
            in
            ( Ready
                { appData
                    | state = nextSharedState
                    , router = nextRouterModel
                }
            , Cmd.map RouterMsg routerCmd
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Ready { state, router } ->
            let
                { title, body } =
                    Router.view state router
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
