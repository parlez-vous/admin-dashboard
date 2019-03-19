module Routes.Admin exposing
  ( Msg
  , update
  , view
  )

import Html exposing (..)

import Routes.Router as Router
import Session

type alias Model = Session.User

type Msg = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )


view : Model -> Router.Details Msg
view model = 
  { title = "Parlez-Vous | Admin Panel"
  , header = div [] [ text "header" ]
  , children = [
      div [] [ text "Admin Panel" ]
    ]
  }
