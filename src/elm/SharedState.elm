module SharedState exposing
  ( SharedState
  , SharedStateUpdate(..)
  , updateSession
  , update
  , init
  )
  

import Browser.Navigation

import Session


type SharedStateUpdate
  = NoUpdate
  | UpdateSession Session.User


type alias SharedState =
  { navKey  : Browser.Navigation.Key
  , session : Session.User
  }


init : Browser.Navigation.Key -> Session.User -> SharedState
init key session =
  { navKey  = key
  , session = session
  }


update : SharedStateUpdate -> SharedState -> SharedState
update updateMsg state =
  case updateMsg of
    UpdateSession session ->
      let 
        _ = Debug.log "Session Updated " session
      in
      { state | session = session }
  
    NoUpdate ->
      state


updateSession : Session.User -> SharedState -> SharedState
updateSession session state =
  update (UpdateSession session) state
