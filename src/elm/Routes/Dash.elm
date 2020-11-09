module Routes.Dash exposing
    ( Model
    , Msg
    , initModel
    , update
    , view
    )

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData
import SharedState exposing (PrivateState, SharedStateUpdate(..), SiteDict)
import UI.Link as Link
import UI.Loader as Loader
import UI.Nav as ResponsiveNav exposing (withVnav)
import UI.Toast as Toast



-- MODEL


type alias Model =
    { toasts : Toast.ToastState
    , navbar : ResponsiveNav.NavState
    }


type Msg
    = ToastMsg Toast.ToastMsg
    | ResponsiveNavMsg ResponsiveNav.Msg


initModel : Model
initModel =
    { toasts = Toast.init
    , navbar = ResponsiveNav.init
    }


update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update _ msg model =
    case msg of
        -- this gets triggered __some__time__
        -- after a toast gets added to the stack
        -- via `addToast`
        ToastMsg subMsg ->
            let
                ( m, cmd ) =
                    model
                        |> Toast.update ToastMsg subMsg
            in
            ( m
            , cmd
            , NoUpdate
            )

        ResponsiveNavMsg subMsg ->
            ResponsiveNav.update subMsg model


viewDash : SiteDict -> Html Msg
viewDash sites =
    let
        registerSiteLink =
            Link.link Link.RegisterSite "registering"
                |> Link.toHtml

        content =
            if Dict.isEmpty sites then
                [ text "hmm... it's awefully quite around here ... start by "
                , registerSiteLink
                , text " your site!"
                ]

            else
                [ text "look at you go!" ]
    in
    div [] content


type alias Title =
    String


view : PrivateState -> Model -> ( Title, Html Msg )
view state model =
    let
        welcomeHeader =
            h1 [] [ text "Welcome!" ]

        content =
            case state.sites of
                RemoteData.NotAsked ->
                    Loader.donut

                RemoteData.Loading ->
                    Loader.donut

                RemoteData.Success sites ->
                    viewDash sites

                RemoteData.Failure _ ->
                    div [] [ text "woopsies!" ]

        viewWithNav =
            withVnav state model ResponsiveNavMsg

        html =
            viewWithNav
                (div [ class "my-5 mx-8" ]
                    [ welcomeHeader
                    , content
                    , Toast.view ToastMsg model.toasts
                    ]
                )
    in
    ( "Admin Panel", html )
