port module Utils exposing (logout)

import Session
import SharedState exposing (SharedStateUpdate(..))

port removeToken : () -> Cmd msg

logout : ( SharedStateUpdate, Cmd msg )
logout =
  ( SharedState.UpdateSession Session.Guest
  , removeToken ()
  )
