port module Utils exposing
  ( logout
  , getApi
  , getNavKey
  )

import Browser.Navigation as Nav

import SharedState exposing (SharedStateUpdate(..), SharedState(..), PublicState)

port removeToken : () -> Cmd msg


-- TODO: turn into extensible record
getApi : SharedState -> String
getApi sharedState =
  case sharedState of
    Public { api } -> api
    Private { api } -> api


getNavKey : SharedState -> Nav.Key
getNavKey sharedState =
  case sharedState of
    Public { navKey } -> navKey
    Private { navKey } -> navKey


logout : PublicState -> ( Cmd msg, SharedStateUpdate )
logout publicState =
  ( removeToken ()
  , SharedState.LogOut publicState
  )
