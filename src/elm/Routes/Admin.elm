module Routes.Admin exposing
  ( Model
  , Msg
  , init
  , update
  , view
  )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url

import Api.Deserialize as Api
import Icons exposing (bell, logo, user)
import Session
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Utils exposing (logout)


-- MODEL

type alias Model =
  { validHostName : Bool
  }


type Msg
  = LogOut
  | SiteInput String




init : Model
init = { validHostName = False }


update : SharedState -> Msg -> Model -> ( Model, Cmd msg, SharedStateUpdate )
update state msg model =
  case msg of
    LogOut ->
      let
        ( logOutCmd, sharedStateUpdate ) = logout

      in
        ( model
        , Cmd.batch [ logOutCmd, Nav.pushUrl state.navKey "/" ]
        , sharedStateUpdate
        )

    SiteInput rawDomain ->
      let
        withProtocol = "https://" ++ rawDomain

        validHostName = case Url.fromString withProtocol of
          Nothing -> False
          Just _ -> True

        newModel = { model | validHostName = validHostName }
      in
        ( newModel, Cmd.none, NoUpdate )




type alias Title = String

view : Api.Admin -> Model -> (Title, Html Msg)
view admin model = 
  let
    welcomeMsg = "Hello " ++ admin.username ++ "! Looks like you haven't registered any sites yet."

    html =
      div [ class "admin-page vertical-nav-container" ]

        -- Vertical Nav
        [ nav [ class "vertical-navbar" ]
            [ div [ class "logo-container" ] [ logo "50" ]
            , div [ class "nav-primary-content"] [  ]
            
            , div [ class "nav-secondary-content" ]
                [ bell
                , div [ class "user-container" ] [ user ] 
                ] 
            ]

        -- Content
        , div [ class "content" ]
          [ h1 [] [ text "Websites" ]
          , div [] [ text welcomeMsg ]
          , input
              [ type_ "text"
              , class "site-input"
              , placeholder "enter a domain name ..."
              , onInput SiteInput
              ] []
          , button [ class "submit", disabled <| not model.validHostName ]
              [ text "submit" ]
          , button [ class "logout", onClick LogOut ] [ text "Log Out" ]
          ]
        ]

  in 
  ( "Admin Panel", html )
