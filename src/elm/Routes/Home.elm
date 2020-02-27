module Routes.Home exposing
  ( Model
  , Msg
  , initModel
  , update
  , view
  )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

import SharedState exposing (SharedState(..), SharedStateUpdate)
import UI.Button as Btn exposing (link)
import UI.Nav exposing (withHnav)
import UI.Link as Link
import Utils exposing (logout, getNavKey)









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

            Private privateState ->
              logout privateState

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
          [ Btn.toHtml <| link Link.Login "log in"
          , Btn.toHtml <| link Link.Signup "sign up"
          ]


    html =
      withHnav
        ctaButtons
        [ h1 [ class "center-text slogan" ] [ text "Enable Conversations"]
        , pre [ class "center-text" ] [ text "work in progress" ]
        , p [ class "center-text" ] [ text "The fastest way to engage your audience" ]
        ]

  in
    ( "Home", html )

