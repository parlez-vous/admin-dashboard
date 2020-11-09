module Routes.Home exposing
    ( Model
    , Msg
    , initModel
    , update
    , view
    )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import SharedState exposing (SharedState(..), SharedStateUpdate)
import UI.Link as Link
import UI.Nav exposing (withHnav)
import Utils exposing (getNavKey, logout)



-- MSG


type Msg
    = GoToDashboard
    | LogOut


type alias Model =
    ()


initModel : Model
initModel =
    ()



-- UPDATE


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        GoToDashboard ->
            let
                navKey =
                    getNavKey sharedState
            in
            ( model
            , Nav.pushUrl navKey "/dash"
            , SharedState.NoUpdate
            )

        LogOut ->
            let
                ( cmd, sharedStateUpdate ) =
                    case sharedState of
                        -- logging out from an unauthenticated state
                        -- does not make sense
                        Public _ ->
                            ( Cmd.none, SharedState.NoUpdate )

                        Private privateState ->
                            logout privateState
            in
            ( model
            , cmd
            , sharedStateUpdate
            )



-- View


type alias Title =
    String


view : SharedState -> Model -> ( Title, Html Msg )
view sharedState model =
    let
        a =
            Debug.log "Shared State: "
                (case sharedState of
                    Private _ ->
                        "private"

                    Public _ ->
                        "public"
                )

        html =
            div []
                [ h1 [ class "center-text slogan" ] [ text "Enable Conversations" ]
                , pre [ class "center-text" ] [ text "work in progress" ]
                , p [ class "center-text" ] [ text "The fastest way to engage your audience" ]
                ]
    in
    ( "Home", html )
