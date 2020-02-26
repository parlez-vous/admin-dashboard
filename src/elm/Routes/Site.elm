module Routes.Site exposing
  ( Model
  , Msg
  , initModel
  , transitionTrigger
  , update
  , view
  )


import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import RemoteData exposing (WebData)

import Api
import Api.Deserialize as Input
import SharedState exposing (PrivateState, SharedStateUpdate)
import UI.Nav as ResponsiveNav exposing (withVnav)
import Utils as Utils


type alias Model =
  { siteId : String
  , navbar : ResponsiveNav.NavState
  }



type Msg
  = SiteResponse (WebData Input.Site)
  | ResponsiveNavMsg ResponsiveNav.Msg


initModel : String -> Model
initModel siteId = Model siteId ResponsiveNav.init


transitionTrigger : PrivateState -> Model -> Cmd Msg
transitionTrigger { admin, api, sites } model =
  let
    ( _, token ) = admin
  in
    case sites of
      RemoteData.NotAsked -> 
        Api.getSingleSite token api model.siteId SiteResponse
      
      _ -> Cmd.none




update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case msg of
    SiteResponse response ->
      case response of
        RemoteData.Success site ->
          ( model, Cmd.none, SharedState.InsertSite site)

        _ -> ( model, Cmd.none, SharedState.NoUpdate )

    ResponsiveNavMsg subMsg ->
      ResponsiveNav.update subMsg model


viewSiteNotVerifiedWarning : Input.Site -> Html Msg
viewSiteNotVerifiedWarning site =
  let
    styles =
        [ "border"
        , "border-solid"
        , "rounded"
        , "border-gray-400"
        , "w-full"
        , "mt-8"
        , "p-5"
        , "md:w-1/2"
        , "md:mx-auto"
        ]
  in
    div [ Utils.toClass styles ]
      [ h2 [] [ text "Site Not Verified!" ]
      , p [] [ text "please add the following TXT record to your site:" ]
      , div [] [ text site.dnsTag ]
      ]

viewSite : Input.Site -> Html Msg
viewSite site =
  if site.verified then
    div [] [ text "ayyy you all good!" ]
  else
    viewSiteNotVerifiedWarning site
    

type alias Title = String

view : PrivateState -> Model -> (Title, Html Msg)
view state model =
  let
    viewWithNav = withVnav state model ResponsiveNavMsg

    maybeSite = case state.sites of 
      RemoteData.Success sites_ ->
        Dict.get model.siteId sites_

      _ -> Nothing

    info = case maybeSite of
      Nothing -> text "site not found"
      Just site ->
        div [ class "m-2 md:m-12 w-full" ]
          [ h1 [ class "text-2xl" ] [ text site.hostname ]
          , viewSite site
          ]
  in
  ( "Site"
  , viewWithNav info
  )
