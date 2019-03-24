module Routes.Admin exposing
  ( Msg
  , view
  )

import Html exposing (..)

import Session

type Msg = NoOp

type alias Title = String

view : (Title, Html Msg)
view = 
  let
    html = div [] [ text "Admin Panel" ]

  in 
  ( "Admin Panel", html )
