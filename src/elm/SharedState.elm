module SharedState exposing
  ( SharedState(..)
  , SharedStateUpdate(..)
  , PrivateState
  , PublicState
  , update
  , init
  , toPrivate
  )
  

import Browser.Navigation
import RemoteData exposing (WebData)

import Api.Deserialize as Input



type SharedStateUpdate
  = NoUpdate
  | SetAdmin Input.AdminWithToken
  | UpdateSites Input.Sites
  | LogOut PublicState


type alias PublicState =
  { navKey  : Browser.Navigation.Key
  , api     : String
  }

type alias PrivateState =
  { navKey  : Browser.Navigation.Key
  , api     : String
  , admin   : Input.AdminWithToken
  , sites   : WebData Input.Sites
  }


type SharedState
  = Public PublicState
  | Private PrivateState
  


toPrivate : Input.AdminWithToken -> PublicState -> PrivateState
toPrivate admin publicState =
  { navKey = publicState.navKey
  , api = publicState.api
  , admin = admin
  , sites = RemoteData.NotAsked
  }


init : Browser.Navigation.Key -> String -> PublicState
init key api =
  { navKey  = key
  , api     = api
  }

update : SharedStateUpdate -> SharedState -> SharedState
update updateMsg state =
  case updateMsg of
    -- called when user logs in
    SetAdmin admin ->
      case state of
        Private privateState ->
          state

        Public { navKey, api } ->
          Private
            { navKey = navKey
            , api = api
            , admin = admin
            , sites = RemoteData.NotAsked
            }
    
    UpdateSites sites ->
      case state of
        Public _ -> state

        Private privateState -> 
          Private
            { privateState
              | sites = RemoteData.Success sites
            }


    LogOut publicState ->
      Public publicState

    NoUpdate ->
      state
