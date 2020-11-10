{--
Horizontal Nav
--}


module UI.Nav exposing
    ( Msg
    , NavState
    , init
    , update
    , withHnav
    , withVnav
    )

import Ant.Button as Btn exposing (button)
import Ant.Typography.Text as Text
import Browser.Navigation as Nav
import Dict
import Html exposing (Html, div, header, nav, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import RemoteData
import SharedState exposing (PrivateState, SharedStateUpdate)
import UI.Icons exposing (hamburger, logo, x)
import UI.Link as Link
import UI.Loader as Loader
import Utils as Utils


type alias NavState =
    { responsiveNavVisible : Bool
    , siteSummaryVisible : Bool
    }


type alias WithNavbar a =
    { a
        | navbar : NavState
    }


type Msg
    = LogOut PrivateState
    | ToggleResponsiveNavbar


init : NavState
init =
    { responsiveNavVisible = True
    , siteSummaryVisible = False
    }


update : Msg -> WithNavbar a -> ( WithNavbar a, Cmd msg, SharedStateUpdate )
update msg ({ navbar } as parentModel) =
    case msg of
        ToggleResponsiveNavbar ->
            ( { parentModel
                | navbar =
                    { navbar
                        | responsiveNavVisible = not navbar.responsiveNavVisible
                    }
              }
            , Cmd.none
            , SharedState.NoUpdate
            )

        LogOut privateState ->
            let
                ( logOutCmd, sharedStateUpdate ) =
                    Utils.logout privateState
            in
            ( parentModel
            , Cmd.batch [ logOutCmd, Nav.pushUrl privateState.navKey "/" ]
            , sharedStateUpdate
            )


hnav : List (Html msg) -> Html msg
hnav children =
    header [ class "w-auto h-20 pt-6 pb-2 px-4 flex justify-between" ]
        [ logo "45"
        , nav [] children
        ]


withVnav : PrivateState -> { a | navbar : NavState } -> (Msg -> msg) -> Html msg -> Html msg
withVnav state { navbar } tagger pageContent =
    let
        loading =
            div [ class "loading-container" ] [ Loader.donut ]

        siteNav =
            case state.sites of
                RemoteData.Success sites ->
                    Dict.values sites
                        |> List.map
                            (\site ->
                                Text.text site.hostname
                                    |> Text.withType (Text.Link (Link.toHref <| Link.Site site.id) Text.Self)
                                    |> Text.toHtml
                            )
                        |> div []

                _ ->
                    loading

        navTopContents =
            [ logo "40"
            , siteNav
            ]

        navBottomContents =
            [ button "Log Out"
                |> Btn.onClick (tagger <| LogOut state)
                |> Btn.toHtml
            ]

        navContent =
            div [ class "font-bold flex flex-col items-center h-full" ]
                [ div [ class "flex flex-col justify-between h-full" ]
                    [ div [] navTopContents
                    , div [] navBottomContents
                    ]
                ]

        regularNav =
            header
                [ class "hidden bg-gray-200 h-full w-48 p-5 md:block"
                ]
                [ navContent ]

        closeIcon =
            div [ onClick (tagger ToggleResponsiveNavbar) ]
                [ x ]

        responsiveNav =
            if navbar.responsiveNavVisible then
                header
                    [ class "bg-gray-100 fixed h-full w-2/3 p-5 md:hidden"
                    ]
                    [ closeIcon, navContent ]

            else
                div
                    [ class "m-5 flex justify-between md:hidden"
                    , onClick (tagger ToggleResponsiveNavbar)
                    ]
                    [ hamburger ]
    in
    div [ class "h-full w-full md:flex" ]
        [ responsiveNav
        , regularNav
        , pageContent
        ]


withHnav : List (Html msg) -> List (Html msg) -> Html msg
withHnav items content =
    div [ class "h-full w-full" ]
        [ hnav items
        , div [ class "w-full mx-4 md:mx-auto text-center" ]
            content
        ]
