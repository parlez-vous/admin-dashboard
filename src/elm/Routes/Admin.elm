module Routes.Admin exposing
  ( Msg
  , view
  )

import Html exposing (..)

import Session
import SharedState exposing (SharedState)

type Msg = NoOp

type alias Title = String

view : SharedState -> (Title, Html Msg)
view sharedState = 
  let
    welcomeMsg =
      case sharedState.session of
        Session.Guest -> "Hello stranger!"
        Session.Admin (admin, _) -> "Hello " ++ admin.username ++ "!"

    html =
      div []
        [ h1 [] [ text "Admin Panel" ]
        , div [] [ text welcomeMsg ]
        ]

  in 
  ( "Admin Panel", html )
