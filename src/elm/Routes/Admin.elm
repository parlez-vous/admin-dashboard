module Routes.Admin exposing
  ( Msg
  , update
  , view
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Api.Deserialize as Api
import Session
import SharedState exposing (SharedStateUpdate(..))
import Utils exposing (logout)


type Msg = LogOut



update : Msg -> ( Cmd msg, SharedStateUpdate )
update _ = logout
  


type alias Title = String

view : Api.Admin -> (Title, Html Msg)
view admin = 
  let
    welcomeMsg = "Hello " ++ admin.username ++ "!"

    html =
      div [ class "admin-page" ]
        [ nav [ class "navbar" ]
            [ button [ onClick LogOut ] [ text "Log Out" ] ]
        , h1 [] [ text "Admin Panel" ]
        , div [] [ text welcomeMsg ]
        ]

  in 
  ( "Admin Panel", html )
