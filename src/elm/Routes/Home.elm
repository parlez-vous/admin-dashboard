module Routes.Home exposing
  ( Model
  , Msg
  , initModel
  , update
  , view
  )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (..)
import Http


import Api
import Api.Output as Output
import Api.Deserialize as Input
import UI.Icons exposing (logo)
import Utils exposing (logout, getApi, getNavKey)
import SharedState exposing (SharedState(..), SharedStateUpdate)
import UI.Loader as Loader
import UI.Button as Btn exposing (link)
import UI.Hnav exposing (hnav)



-- PORTS





-- MSG



type Msg
  = GoToDashboard
  | LogOut


type alias Model = ()

initModel : Model
initModel = ()


-- UPDATE

update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
  case msg of
    GoToDashboard ->
      let
        navKey = getNavKey sharedState
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
            Public _ -> ( Cmd.none, SharedState.NoUpdate )

            Private { api, navKey } ->
              logout { api = api, navKey = navKey }

      in
      ( model
      , cmd
      , sharedStateUpdate
      )







-- View

type alias Title = String

view : SharedState -> Model -> (Title, Html Msg)
view sharedState model =
  let
    ctaButtons =
      case sharedState of
        Private _ -> 
          [ button [ onClick LogOut ] [ text "log out" ]
          , Btn.button "Go To Dashboard"
            |> Btn.onClick GoToDashboard
            |> Btn.toHtml
          ]

        Public _ ->
          [ Btn.toHtml <| link Btn.Login "log in"
          , Btn.toHtml <| link Btn.Signup "sign up"
          ]


    html =
      div [ class "home-page" ]
        [ hnav ctaButtons
        , div [ class "container" ]
            [ div [ class "row" ]
                [ h1 [ class "center-text slogan" ] [ text "Enable Conversations"]
                , pre [ class "center-text" ] [ text "work in progress" ]
                , div [ class "logo-container" ] [ logo "125" ]
                , p [ class "center-text" ] [ text "The fastest way to engage your audience" ]
                ]
            ]
        ]

  in
    ( "Home", html )

