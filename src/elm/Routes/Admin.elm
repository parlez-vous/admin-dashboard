module Routes.Admin exposing
  ( Msg
  , update
  , view
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Session
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Utils exposing (logout)


type Msg = LogOut



update : Msg -> ( Cmd msg, SharedStateUpdate )
update _ = logout
  


type alias Title = String

view : SharedState -> (Title, Html Msg)
view sharedState = 
  let
    welcomeMsg =
      case sharedState.session of
        Session.Guest -> "Hello stranger!"
        Session.Admin (admin, _) -> "Hello " ++ admin.username ++ "!"

    html =
      div [ class "admin-page" ]
        [ nav [ class "navbar" ]
            [ button [ onClick LogOut ] [ text "Log Out" ] ]
        , h1 [] [ text "Admin Panel" ]
        , div [] [ text welcomeMsg ]
        ]

  in 
  ( "Admin Panel", html )
