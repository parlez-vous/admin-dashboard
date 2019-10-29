port module Utils exposing
  ( logout
  , getApi
  )

import SharedState exposing (SharedStateUpdate(..), SharedState(..), PublicState)

port removeToken : () -> Cmd msg


-- TODO: turn into extensible record
getApi : SharedState -> String
getApi sharedState =
  case sharedState of
    Public { api } -> api
    Private { api } -> api


logout : PublicState -> ( Cmd msg, SharedStateUpdate )
logout publicState =
  ( removeToken ()
  , SharedState.LogOut publicState
  )
