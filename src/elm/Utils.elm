port module Utils exposing (logout)

import Session
import SharedState exposing (SharedStateUpdate(..))

port removeToken : () -> Cmd msg

logout : ( Cmd msg, SharedStateUpdate )
logout =
  ( removeToken ()
  , SharedState.UpdateSession Session.Guest
  )
