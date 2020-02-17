module Routes.RegisterSite exposing
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
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Toasty
import RemoteData as RemoteData
import SharedState exposing (PrivateState, SharedStateUpdate(..))

import Api
import Api.Deserialize as Input
import Api.Output as Output
import UI.Button as Btn
import UI.Icons exposing (logo, rightCaret)
import UI.Input as Input
import UI.Toast as Toast
import UI.Loader as Loader
import UI.Nav as ResponsiveNav exposing (withVnav)
import Utils as Utils


type alias Model =
  { hostname : String
  , toasties : Toast.ToastState
  , responsiveNavVisible : Bool
  , siteSummaryVisible : Bool
  }

type Msg
  = DomainInput String
  | SubmitDomain String  
  | DomainSubmitted (Result Http.Error Input.Site)
  | ToastMsg (Toasty.Msg String)
  | ResponsiveNavMsg ResponsiveNav.Msg
  | LogOut
  | ToggleShowSiteSummary
  


initModel : Model
initModel =
  { hostname = ""
  , toasties = Toast.init 
  , responsiveNavVisible = True
  , siteSummaryVisible = False
  }

transitionTrigger : PrivateState -> Cmd Msg
transitionTrigger _ = Cmd.none


toastBuilder : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
toastBuilder = Toasty.addToast Toast.config ToastMsg


update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case msg of
    -- FIXME: move into navbar UI
    LogOut ->
      let
        publicState = { api = state.api, navKey = state.navKey }
        ( logOutCmd, sharedStateUpdate ) = Utils.logout publicState

      in
        ( model
        , Cmd.batch [ logOutCmd, Nav.pushUrl state.navKey "/" ]
        , sharedStateUpdate
        )

    DomainInput rawDomain ->
      ( { model | hostname = rawDomain
        }
      , Cmd.none
      , NoUpdate
      )

    SubmitDomain rawDomain ->
      let
        data = Output.RegisterSite rawDomain

        ( _, token ) = state.admin

      in
        ( model
        , Api.registerSite token state.api DomainSubmitted data
        , NoUpdate 
        )

    DomainSubmitted result ->
      case result of
        Ok site ->
          let
            _ = Debug.log "Site registered: " site
          in
            ( model, Cmd.none, NoUpdate )

        Err e ->
         {--
         ( m, c ) = ( model, Cmd.none )
            |> Toasty.addToast Toast.config ToastMsg "Invalid URL"
         --}
          let
            _ = Debug.log "Failed to register site: " e

            ( newModel, cmd ) =
              case e of
                Http.BadStatus statusCode ->
                  if statusCode == 400 then
                    ( model, Cmd.none )
                      |>  toastBuilder "Make sure you enter a Fully Qualified Domain Name!" 
                  else if statusCode == 409 then
                    ( model, Cmd.none )
                      |> toastBuilder "This Site is already registered!"
                  else
                    ( model, Cmd.none )
                    |> toastBuilder "Something went wrong"

                _ -> ( model, Cmd.none )
                  |> toastBuilder "Something went wrong"
          in
            ( newModel, cmd, NoUpdate )

    ToggleShowSiteSummary ->
      let
        newModel = { model | siteSummaryVisible = not model.siteSummaryVisible }
      in
      (newModel, Cmd.none, NoUpdate )
    
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

    ResponsiveNavMsg subMsg ->
      let
        newModel = ResponsiveNav.update subMsg model
      in
        ( newModel, Cmd.none, NoUpdate )


type alias Title = String

view : PrivateState -> Model -> (Title, Html Msg)
view state model =
  let
    loading =
      div [ class "loading-container" ]
        [ Loader.donut ]

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
        , Btn.button "Log Out"
          |> Btn.onClick LogOut
          |> Btn.toHtml
        ]

    content =
      div []
        [ Input.input (Input.Url model.hostname DomainInput)
          |> Input.toHtml
        ]

    html =
      viewWithNav
        navigationOpts
        (div [ class "my-5 mx-8" ]
          [ content
          , Toast.view ToastMsg model.toasties
          ])
  in
  
  ( "Register Site", html )
  