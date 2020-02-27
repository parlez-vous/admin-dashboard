module Routes.Site exposing
  ( Model
  , Msg
  , initModel
  , update
  , view
  )


import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import RemoteData

import Api.Deserialize as Input
import SharedState exposing (PrivateState, SharedStateUpdate)
import UI.Nav as ResponsiveNav exposing (withVnav)
import Utils as Utils


type alias Model =
  { siteId : String
  , navbar : ResponsiveNav.NavState
  }



type Msg
  = ResponsiveNavMsg ResponsiveNav.Msg


initModel : String -> Model
initModel siteId = Model siteId ResponsiveNav.init


update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update _ msg model =
  case msg of
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
