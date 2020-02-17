module Routes.Dash exposing
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
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Toasty
import RemoteData exposing (WebData)

import Api
import Api.Deserialize as Input
import SharedState exposing (PrivateState, SharedStateUpdate(..), SiteDict)
import Utils exposing (logout)
import UI.Icons exposing (logo, rightCaret)
import UI.Input as Input
import UI.Loader as Loader
import UI.Toast as Toast
import UI.Link as Link
import UI.Nav as ResponsiveNav exposing (withVnav)


-- MODEL

type alias Model =
  { toasties : Toast.ToastState
  , responsiveNavVisible : Bool
  , siteSummaryVisible : Bool
  }


type Msg
  = LogOut
  | ToastMsg (Toasty.Msg String)
  | SitesResponse (WebData Input.Sites)
  | ResponsiveNavMsg ResponsiveNav.Msg
  | ToggleShowSiteSummary



initModel : Model
initModel =
  { toasties = Toast.init
  , responsiveNavVisible = False
  , siteSummaryVisible = False
  }


transitionTrigger : PrivateState -> Cmd Msg
transitionTrigger { admin, api } =
  let
    ( _, token ) = admin
  in
    Api.getSites token api SitesResponse

toastBuilder : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
toastBuilder = Toasty.addToast Toast.config ToastMsg

update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case msg of
    LogOut -> 
      let
        publicState = { api = state.api, navKey = state.navKey }
        ( logOutCmd, sharedStateUpdate ) = logout publicState

      in
        ( model
        , Cmd.batch [ logOutCmd, Nav.pushUrl state.navKey "/" ]
        , sharedStateUpdate
        )

    -- this gets triggered __some__time__
    -- after a toast gets added to the stack
    -- via `addToast`
    ToastMsg subMsg ->
      let
        ( m , cmd ) =
          model
          |> Toasty.update Toast.config ToastMsg subMsg

      in
        ( m
        , cmd
        , NoUpdate
        )

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
              |> toastBuilder "Something went wrong"
          in
          (newModel, cmd, NoUpdate)

        _ ->
          ( model, Cmd.none, NoUpdate )

    ResponsiveNavMsg subMsg ->
      let
        newModel = ResponsiveNav.update subMsg model
      in
        ( newModel, Cmd.none, NoUpdate )

    ToggleShowSiteSummary ->
      let
        newModel = { model | siteSummaryVisible = not model.siteSummaryVisible }
      in
      (newModel, Cmd.none, NoUpdate )
  




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
    loading =
      div [ class "loading-container" ]
        [ Loader.donut ]

    _ = Debug.log "Site State:" state.sites

    content =
      case state.sites of
        RemoteData.NotAsked -> loading
        RemoteData.Loading  -> loading
        RemoteData.Success sites -> viewDash sites
        RemoteData.Failure _ -> div [] [ text "woopsies!" ]
            
    viewWithNav = withVnav model ResponsiveNavMsg

    siteNav =
      case state.sites of
        RemoteData.Success sites ->
          let 
            bgColour = if SharedState.allVerified sites
              then
                "bg-gray-400"
              else
                "bg-red-300"

            classes = Utils.toClass
              [ "inline-block"
              , "bg-gray-400"
              , "py-1"
              , "px-2"
              , "text-center"
              , "rounded"
              , bgColour
              ]

            siteList =
              Dict.values sites
              |> List.map (\site -> text site.hostname)


            siteSummary =
              let
                defaultClasses = [ "site-summary" ]
                
                siteSummaryClasses =
                  if model.siteSummaryVisible then
                    defaultClasses
                  else
                    "hidden" :: defaultClasses

              in
              div [ Utils.toClass siteSummaryClasses ]
                [ div [ class "p-1 border-b-2 border-solid border-gray-300" ] [ text "Your Sites" ]
                , div [] siteList
                ]
          in
            div [ class "relative" ]
              [ div [ onClick ToggleShowSiteSummary, class "mx-5 my-2 p-2 text-center rounded cursor-pointer hover:bg-gray-300 select-none" ]
                  [ div [ ] [ text "Sites ", rightCaret ]
                  , div [ classes ] [ text <| String.fromInt (Dict.size sites) ]
                  ]
              , siteSummary
              ]
        
        _ -> loading

    navigationOpts = 
      div [ class "font-bold" ]
        [ logo "40"
        , siteNav
        , button [ class "", onClick LogOut ] [ text "Log Out" ]
        ]

    html =
      viewWithNav
        navigationOpts
        (div [ class "my-5 mx-8" ]
          [ content
          , Toast.view ToastMsg model.toasties
          ])
  in 
  ( "Admin Panel", html )
