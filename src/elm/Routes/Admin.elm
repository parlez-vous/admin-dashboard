module Routes.Admin exposing
  ( Msg
  , update
  , view
  )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Api.Deserialize as Api
import Icons exposing (bell, logo, user)
import Session
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Utils exposing (logout)


type Msg = LogOut



update : SharedState -> Msg -> ( Cmd msg, SharedStateUpdate )
update state _ =
  let
    ( logOutCmd, sharedStateUpdate ) = logout

  in
    ( Cmd.batch [ logOutCmd, Nav.pushUrl state.navKey "/" ]
    , sharedStateUpdate
    )



type alias Title = String

view : Api.Admin -> (Title, Html Msg)
view admin = 
  let
    welcomeMsg = "Hello " ++ admin.username ++ "!"

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

            , button [ onClick LogOut ] [ text "Log Out" ]
            ]

        -- Content
        , div [ class "content" ]
          [ h1 [] [ text "Admin Panel" ]
          , div [] [ text welcomeMsg ]
          ]
        ]

  in 
  ( "Admin Panel", html )
