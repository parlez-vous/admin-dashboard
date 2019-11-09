module SharedState exposing
  ( SharedState(..)
  , SharedStateUpdate(..)
  , PrivateState
  , PublicState
  , update
  , init
  , toPrivate
  , toDict
  , SiteDict
  )
  

import Browser.Navigation
import RemoteData exposing (WebData)
import Dict exposing (Dict)

import Api.Deserialize as Input


type alias SiteDict = Dict Int Input.Site

toDict : Input.Sites -> SiteDict
toDict =
  List.foldl (\site -> Dict.insert site.id site) Dict.empty


type SharedStateUpdate
  = NoUpdate
  | SetAdmin Input.AdminWithToken
  | UpdateSites SiteDict
  | InsertSite Input.Site
  | LogOut PublicState


type alias PublicState =
  { navKey  : Browser.Navigation.Key
  , api     : String
  }

type alias PrivateState =
  { navKey  : Browser.Navigation.Key
  , api     : String
  , admin   : Input.AdminWithToken
  , sites   : WebData SiteDict
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

    InsertSite site ->
      case state of
        Public _ -> state

        Private privateState ->
          let
            sites = case privateState.sites of
              RemoteData.Success sites_ -> sites_
              _ -> Dict.empty

          in
            Private
              { privateState
                | sites = RemoteData.Success <| Dict.insert site.id site sites
              }

    LogOut publicState ->
      Public publicState

    NoUpdate ->
      state
