module Router exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Api
import Api.Deserialize as Input
import Browser.Navigation as Nav
import Html as Html exposing (..)
import Html.Attributes exposing (class)
import RemoteData exposing (WebData)
import Routes.Dash as Dash
import Routes.Home as Home
import Routes.RegisterSite as RegisterSite
import Routes.Site as Site
import SharedState exposing (PrivateState, SharedState(..), SharedStateUpdate)
import UI.Toast as Toast
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, string)


type Route
    = Home Home.Model
    | Dash Dash.Model
    | Site Site.Model
    | RegisterSite RegisterSite.Model
    | NotFound


type alias Model =
    { activeRoute : Route
    , toasts : Toast.ToastState
    }


type Msg
    = UrlChange Url
    | ToastMsg Toast.ToastMsg
    | SitesResponse (WebData Input.Sites)
    | HomeMsg Home.Msg
    | DashMsg Dash.Msg
    | SiteMsg Site.Msg
    | RegisterSiteMsg RegisterSite.Msg


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map (Home Home.initModel) Parser.top
        , Parser.map (Dash Dash.initModel) (Parser.s "dash")
        , Parser.map (Site << Site.initModel) (Parser.s "sites" </> string)

        --, Parser.map (Login Login.initModel) (Parser.s "login")
        --, Parser.map (Signup Signup.initModel) (Parser.s "signup")
        , Parser.map (RegisterSite RegisterSite.initModel) (Parser.s "register-site")
        ]


fromUrl : Url -> Route
fromUrl =
    Maybe.withDefault NotFound << Parser.parse parser


init : Url -> SharedState -> ( Model, Cmd Msg )
init url sharedState =
    let
        route =
            fromUrl url
    in
    ( { activeRoute = route, toasts = Toast.init }
    , transitionTrigger route sharedState
    )


{-| trigger commands on page transitions
and initializations
-}
transitionTrigger : Route -> SharedState -> Cmd Msg
transitionTrigger route state =
    case ( route, state ) of
        -- redirect guests on private routes
        ( Dash _, Public { navKey } ) ->
            Nav.pushUrl navKey "/"

        ( Site _, Public { navKey } ) ->
            Nav.pushUrl navKey "/"

        ( RegisterSite _, Public { navKey } ) ->
            Nav.pushUrl navKey "/"

        -- redirect authed users away from public routes
        ( Home _, Private { navKey } ) ->
            Nav.pushUrl navKey "/dash"

        ( _, Private { admin, api, sites } ) ->
            let
                ( _, token ) =
                    admin

                { getManySites } =
                    Api.getApiClient api
            in
            case sites of
                RemoteData.NotAsked ->
                    getManySites token SitesResponse

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
    case ( msg, model.activeRoute ) of
        ( UrlChange url, _ ) ->
            let
                newRoute =
                    fromUrl url

                transitionTriggerMsg =
                    transitionTrigger newRoute state
            in
            ( { model | activeRoute = newRoute }
            , transitionTriggerMsg
            , SharedState.NoUpdate
            )

        ( SitesResponse response, _ ) ->
            case response of
                RemoteData.Success sites ->
                    ( model
                    , Cmd.none
                    , SharedState.UpdateSites <| SharedState.toDict sites
                    )

                RemoteData.Failure _ ->
                    let
                        ( newModel, cmd ) =
                            ( model, Cmd.none )
                                |> Toast.addToast ToastMsg "Something went wrong"
                    in
                    ( newModel, cmd, SharedState.NoUpdate )

                _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )

        ( HomeMsg homeMsg, Home homeModel ) ->
            let
                ( newHomeModel, homeCmd, sharedStateUpdate ) =
                    Home.update state homeMsg homeModel
            in
            ( { model | activeRoute = Home newHomeModel }
            , Cmd.map HomeMsg homeCmd
            , sharedStateUpdate
            )

        ( DashMsg dashMsg, Dash dashModel ) ->
            case state of
                Private privateState ->
                    let
                        ( newDashModel, dashCmd, sharedStateUpdate ) =
                            Dash.update privateState dashMsg dashModel
                    in
                    ( { model | activeRoute = Dash newDashModel }
                    , Cmd.map DashMsg dashCmd
                    , sharedStateUpdate
                    )

                Public _ ->
                    ( { model | activeRoute = Dash dashModel }
                    , Cmd.none
                    , SharedState.NoUpdate
                    )

        ( SiteMsg siteMsg, Site siteModel ) ->
            case state of
                Private privateState ->
                    let
                        ( newSiteModel, siteCmd, sharedStateUpdate ) =
                            Site.update privateState siteMsg siteModel
                    in
                    ( { model | activeRoute = Site newSiteModel }
                    , Cmd.map SiteMsg siteCmd
                    , sharedStateUpdate
                    )

                Public _ ->
                    ( model
                    , Cmd.none
                    , SharedState.NoUpdate
                    )

        ( RegisterSiteMsg registerSiteMsg, RegisterSite registersiteModel ) ->
            case state of
                Private privateState ->
                    let
                        ( newRegisterSiteModel, registerSiteCmd, sharedStateUpdate ) =
                            RegisterSite.update privateState registerSiteMsg registersiteModel
                    in
                    ( { model | activeRoute = RegisterSite newRegisterSiteModel }
                    , Cmd.map RegisterSiteMsg registerSiteCmd
                    , sharedStateUpdate
                    )

                Public _ ->
                    ( model
                    , Cmd.none
                    , SharedState.NoUpdate
                    )

        -- Placeholder for now
        _ ->
            ( model, Cmd.none, SharedState.NoUpdate )



{--Generalize the following
  Site.view privateState siteModel
    |> Tuple.mapSecond (Html.map <| SiteMsg siteModel)
    |> Tuple.mapSecond (Html.map toMsg)
--}


type alias Title =
    String


type alias AppView =
    { title : Title
    , body : Html Msg
    }


viewPrivatePage :
    SharedState
    -> (PrivateState -> m -> ( Title, Html msg ))
    -> m
    -> (msg -> Msg)
    -> ( Title, Html Msg )
viewPrivatePage sharedState routeView model tagger =
    let
        redirectPage =
            ( "Redirecting ..."
            , div [] [ text "Redirecting ..." ]
            )
    in
    case sharedState of
        Public _ ->
            redirectPage

        Private privateState ->
            routeView privateState model
                |> Tuple.mapSecond (Html.map tagger)


view : SharedState -> Model -> AppView
view sharedState model =
    let
        toastView =
            Toast.view ToastMsg model.toasts

        viewPrivateRoute =
            viewPrivatePage sharedState

        ( title, html ) =
            case model.activeRoute of
                Home homeModel ->
                    Home.view sharedState homeModel
                        |> Tuple.mapSecond (Html.map HomeMsg)

                Dash dashModel ->
                    viewPrivateRoute
                        Dash.view
                        dashModel
                        DashMsg

                Site siteModel ->
                    viewPrivateRoute
                        Site.view
                        siteModel
                        SiteMsg

                RegisterSite registerSiteModel ->
                    viewPrivateRoute
                        RegisterSite.view
                        registerSiteModel
                        RegisterSiteMsg

                NotFound ->
                    ( "Woops!", div [] [ text "404 Not Found" ] )
    in
    { title = title ++ " | Parlez-Vous "
    , body = div [ class "bg-gray-100" ] [ html, toastView ]
    }
