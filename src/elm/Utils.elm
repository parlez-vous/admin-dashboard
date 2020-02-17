port module Utils exposing
  ( logout
  , getApi
  , getNavKey
  , setToken
  , toClass
  )

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes exposing (class)

import SharedState exposing (SharedStateUpdate(..), SharedState(..), PrivateState)

port removeToken : () -> Cmd msg

port setToken : String -> Cmd msg



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


logout : PrivateState -> ( Cmd msg, SharedStateUpdate )
logout { navKey, api } =
  let
    publicState =
      { navKey = navKey
      , api = api
      }
  in
    ( removeToken ()
    , SharedState.LogOut publicState
    )


toClass : List String -> Attribute msg
toClass list = 
  List.intersperse " " list
  |> String.concat
  |> class
