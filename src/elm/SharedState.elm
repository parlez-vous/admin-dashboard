module SharedState exposing
  ( SharedState
  , SharedStateUpdate(..)
  , update
  , init
  )
  

import Browser.Navigation
import RemoteData exposing (WebData)

import Session



type SharedStateUpdate
  = NoUpdate
  | UpdateSession Session.User


type alias SharedState =
  { navKey  : Browser.Navigation.Key
  , session : WebData Session.User
  , api     : String
  }


init : Browser.Navigation.Key -> String -> SharedState
init key api =
  { navKey  = key
  , session = RemoteData.NotAsked
  , api     = api
  }


update : SharedStateUpdate -> SharedState -> SharedState
update updateMsg state =
  case updateMsg of
    UpdateSession session ->
      let 
        _ = Debug.log "Session Updated " session
      in
      { state | session = RemoteData.Success session }
    
    NoUpdate ->
      state
