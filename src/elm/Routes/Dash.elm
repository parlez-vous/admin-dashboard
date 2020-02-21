module Routes.Dash exposing
  ( Model
  , Msg
  , initModel
  , transitionTrigger
  , update
  , view
  )

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (WebData)

import Api
import Api.Deserialize as Input
import SharedState exposing (PrivateState, SharedStateUpdate(..), SiteDict)
import UI.Input as Input
import UI.Toast as Toast
import UI.Link as Link
import UI.Loader as Loader
import UI.Nav as ResponsiveNav exposing (withVnav)


-- MODEL

type alias Model =
  { toasts : Toast.ToastState
  , navbar   : ResponsiveNav.NavState
  }


type Msg
  = ToastMsg Toast.ToastMsg
  | SitesResponse (WebData Input.Sites)
  | ResponsiveNavMsg ResponsiveNav.Msg



initModel : Model
initModel =
  { toasts = Toast.init
  , navbar = ResponsiveNav.init
  }


transitionTrigger : PrivateState -> Cmd Msg
transitionTrigger { admin, api, sites } =
  let
    ( _, token ) = admin
  in
    case sites of
      RemoteData.NotAsked -> 
        Api.getSites token api SitesResponse
      
      _ -> Cmd.none


update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case msg of
    -- this gets triggered __some__time__
    -- after a toast gets added to the stack
    -- via `addToast`
    ToastMsg subMsg ->
      let
        ( m , cmd ) =
          model
          |> Toast.update ToastMsg subMsg

      in
        ( m
        , cmd
        , NoUpdate
        )

    ResponsiveNavMsg subMsg ->
      ResponsiveNav.update subMsg model

    SitesResponse response ->
      let 
        _ = Debug.log "SitesResponse" response
      in
      case response of
        RemoteData.Success sites ->
          ( model
          , Cmd.none
          , UpdateSites <| SharedState.toDict sites
          )

        RemoteData.Failure _ ->
          let
            (newModel, cmd) = ( model, Cmd.none )
              |> Toast.addToast ToastMsg "Something went wrong"
          in
          (newModel, cmd, NoUpdate)

        _ ->
          ( model, Cmd.none, NoUpdate )

  




viewDash : SiteDict -> Html Msg
viewDash sites =
  let
    registerSiteLink = Link.link Link.RegisterSite "registering"
      |> Link.toHtml

    content = 
      if Dict.isEmpty sites then
        [ text "hmm... it's awefully quite around here ... start by "
        , registerSiteLink
        , text " your site!"
        ]

      else
        [ text "look at you go!" ]
  in
    div [] content


type alias Title = String

view : PrivateState -> Model -> (Title, Html Msg)
view state model = 
  let
    welcomeHeader = h1 [] [ text "Welcome!" ]

    content =
      case state.sites of
        RemoteData.NotAsked -> Loader.donut
        RemoteData.Loading  -> Loader.donut
        RemoteData.Success sites -> viewDash sites
        RemoteData.Failure _ -> div [] [ text "woopsies!" ]
            
    viewWithNav = withVnav state model ResponsiveNavMsg

    html =
      viewWithNav
        (div [ class "my-5 mx-8" ]
          [ welcomeHeader
          , content
          , Toast.view ToastMsg model.toasts
          ])
  in 
  ( "Admin Panel", html )
