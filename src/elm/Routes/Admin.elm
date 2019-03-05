module Routes.Admin exposing ( .. )

import Html exposing (..)

import Routes.Router as Router

type Model = Hello

type Msg
  = SayHello

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( Hello, Cmd.none )


view : Model -> Router.Details Msg
view model = 
  { title = "Parlez-Vous | Admin Panel"
  , header = div [] [ text "header" ]
  , children = [
      div [] [ text "Admin Panel" ]
    ]
  }
