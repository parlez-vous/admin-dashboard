port module Utils exposing (logout)

import SharedState exposing (SharedStateUpdate(..), PublicState)

port removeToken : () -> Cmd msg

logout : PublicState -> ( Cmd msg, SharedStateUpdate )
logout publicState =
  ( removeToken ()
  , SharedState.LogOut publicState
  )
