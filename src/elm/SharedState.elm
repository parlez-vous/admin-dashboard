module SharedState exposing
  ( SharedState
  , SharedStateUpdate(..)
  , update
  , init
  )
  

import Browser.Navigation

import Session
import UI.Toast as Toast


type SharedStateUpdate
  = NoUpdate
  | UpdateSession Session.User
  | UpdateToasts (Toast.ToastState)


type alias SharedState =
  { navKey  : Browser.Navigation.Key
  , session : Session.User
  , api     : String
  , toasts  : Toast.ToastState
  }


init : Browser.Navigation.Key -> Session.User -> String -> SharedState
init key session api =
  { navKey  = key
  , session = session
  , api     = api
  , toasts  = Toast.init
  }


update : SharedStateUpdate -> SharedState -> SharedState
update updateMsg state =
  case updateMsg of
    UpdateSession session ->
      let 
        _ = Debug.log "Session Updated " session
      in
      { state | session = session }
  
    UpdateToasts newToastState ->
      { state | toasts = newToastState }
    
    NoUpdate ->
      state
