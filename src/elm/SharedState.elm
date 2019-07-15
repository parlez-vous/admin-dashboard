module SharedState exposing
  ( SharedState
  , SharedStateUpdate(..)
  , update
  , init
  )
  

import Browser.Navigation
import RemoteData exposing (WebData)

import Session
import Api.Deserialize as Input



type SharedStateUpdate
  = NoUpdate
  | UpdateSession Session.User
  | UpdateSites Input.Sites


type alias SharedState =
  { navKey  : Browser.Navigation.Key
  , api     : String
  , session : WebData Session.User
  , sites   : WebData Input.Sites
  }


init : Browser.Navigation.Key -> String -> SharedState
init key api =
  { navKey  = key
  , api     = api
  , session = RemoteData.NotAsked
  , sites   = RemoteData.NotAsked
  }


update : SharedStateUpdate -> SharedState -> SharedState
update updateMsg state =
  case updateMsg of
    UpdateSession session ->
      let 
        _ = Debug.log "Session Updated " session
      in
      { state | session = RemoteData.Success session }
    
    UpdateSites sites ->
      { state | sites = RemoteData.Success sites }

    NoUpdate ->
      state
