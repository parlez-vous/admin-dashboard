-- TODO

module Site exposing
  ( Model
  , init
  )


import Browser.Navigation as Nav
import SharedState exposing (PrivateState)

import Html exposing (..)


type alias Model =
  { siteId : Int
  }



-- initialize the model
init : Int -> Model
init = Model


-- Get site from SharedState
-- Or issue HTTP req to fetch
-- this specific site
initRoute :
  String ->
  String ->
  Nav.Key ->
  Cmd msg
initRoute token api navKey = Cmd.none



{--
{ navKey  = key
  , api     = api
  , session = RemoteData.NotAsked
--}

type alias Title = String

view : PrivateState -> Model -> (Title, Html msg)
view state model =
  ( "Site"
  , div [ ] [ text (String.fromInt model.siteId) ]
  )
