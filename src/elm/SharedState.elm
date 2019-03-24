module SharedState exposing
  ( SharedState
  , SharedStateUpdate
  , update
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


update : SharedStateUpdate -> SharedState -> SharedState
update updateMsg state =
  case updateMsg of
    UpdateSession session ->
      { state | session = session }
  
    NoUpdate ->
      state

