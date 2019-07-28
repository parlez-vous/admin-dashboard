-- TODO

module Routes.Site exposing
  ( Model
  , Msg
  , initModel
  , transitionTrigger
  , update
  , view
  )


import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import RemoteData exposing (WebData)

import Api
import Api.Deserialize as Input
import SharedState exposing (PrivateState, SharedStateUpdate)


type alias Model =
  { siteId : Int
  }



type Msg = SiteResponse (WebData Input.Site)


initModel : Int -> Model
initModel = Model


transitionTrigger : Model -> PrivateState -> Cmd Msg
transitionTrigger model { admin, api, navKey, sites } =
  case sites of
    RemoteData.NotAsked ->
      let
        (_, token) = admin
      in
        Api.getSingleSite token api SiteResponse


    -- there are 2 cases to handle here:
    --   1. The site in question exists in the list of sites
    --   2. The site in question does not exist in the list of sites
    RemoteData.Success sites_ ->
      let
        (_, token) = admin

        -- search the list for `siteId`
        --   if it exists then don't issue an HTTP request
        --   if it DOES NOT exist then issue an HTTP request
        --     for just that one site
      in
      case Dict.get model.siteId sites_ of
        Just site -> Cmd.none
        Nothing -> 
          Api.getSingleSite token api SiteResponse
    

    -- TODO: handle Loading state
    --   how would you have a loading state when first entering the page?
    --   this could be caused by quickly going back and forth between pages
    -- TODO: handle Failure e state
    _ -> Cmd.none




update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case msg of
    SiteResponse response ->
      case response of
        RemoteData.Success site ->
          ( model, Cmd.none, SharedState.InsertSite site)

        _ -> ( model, Cmd.none, SharedState.NoUpdate )


type alias Title = String

view : PrivateState -> Model -> (Title, Html msg)
view state { siteId } =
  let
    site = case state.sites of 
      RemoteData.Success sites_ ->
        Dict.get siteId sites_

      _ -> Nothing

    info = case site of
      Just site_ -> "Yeyyyy"
      Nothing -> "boooo"
  in
  ( "Site"
  , div [ ] [ text info ]
  )
