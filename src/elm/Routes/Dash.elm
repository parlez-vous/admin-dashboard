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
import Html.Events exposing (onClick, onInput)
import Http
import Url
import Toasty
import RemoteData exposing (WebData)

import Api
import Api.Output as Output
import Api.Deserialize as Input
import SharedState exposing (PrivateState, SharedStateUpdate(..), SiteDict)
import Utils exposing (logout)
import UI.Icons exposing (logo, cog)
import UI.Button as U
import UI.Loader as Loader
import UI.Toast as Toast
import UI.Nav as ResponsiveNav exposing (withVnav)


-- MODEL

type alias Model =
  { hostname : String
  , toasties : Toast.ToastState
  , responsiveNavVisible : Bool
  }


type Msg
  = LogOut
  | SiteInput String
  | SubmitDomain String
  | DomainSubmitted (Result Http.Error Input.Site)
  | ToastMsg (Toasty.Msg String)
  | SitesResponse (WebData Input.Sites)
  | ResponsiveNavMsg ResponsiveNav.Msg



initModel : Model
initModel =
  { hostname = ""
  , toasties = Toast.init
  , responsiveNavVisible = False
  }


transitionTrigger : PrivateState -> Cmd Msg
transitionTrigger { admin, api, navKey } =
  let
    ( _, token ) = admin
  in
    Api.getSites token api SitesResponse


isValidHostname : String -> Bool
isValidHostname rawDomain =
  let 
    withProtocol = "https://" ++ rawDomain

  in
    case Url.fromString withProtocol of
      Nothing -> False
      Just _ -> True


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

    SiteInput rawDomain ->
      ( { model | hostname = rawDomain
        }
      , Cmd.none
      , NoUpdate
      )

    SubmitDomain rawDomain ->
      if not (isValidHostname rawDomain)
      then
        let
          ( m, c ) = ( model, Cmd.none )
            |> Toasty.addToast Toast.config ToastMsg "Invalid URL"
        in
        ( m, c, NoUpdate )
      else
        let
          _ = Debug.log "Submitting domain ..." rawDomain

          withProtocol = "https://" ++ rawDomain

          data = Output.RegisterSite withProtocol

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
          let
            _ = Debug.log "Failed to register site: " e
          in
            ( model, Cmd.none, NoUpdate )

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
      case response of
        RemoteData.Success sites ->
          ( model
          , Cmd.none
          , UpdateSites <| SharedState.toDict sites
          )

        _ ->
          ( model, Cmd.none, NoUpdate )

    ResponsiveNavMsg subMsg ->
      let
        newModel = ResponsiveNav.update subMsg model
      in
        ( newModel, Cmd.none, NoUpdate )
  




type alias Title = String

viewSite : Input.Site -> Html Msg
viewSite site =
  let
    link = "/sites/" ++ String.fromInt site.id

    status = if site.verified
      then "Verified"
      else "Not Verified :("

  in
    div [ class "" ]
      [ header [ class "" ]
          [ a [ href link ] [ h2 [] [ text site.hostname ] ]
          
          -- This is a placeholder for what I imagine
          -- to be site details
          , p [] [ text "No data to display" ]
          ]
      , div [ class "" ]
          [ text status ]
      ]


viewSites : SiteDict -> Html Msg
viewSites sites =
  let
    adminHasSites = (Dict.size sites) > 0

    baseContent =
      List.append
        [ text <| "You have " ++ (String.fromInt <| Dict.size sites) ++ " sites!"
        ]
        (List.map viewSite (Dict.values sites))

    content =
      if adminHasSites then
        baseContent
      else
        List.append baseContent [ text "heyooo" ]


  in
    div [] content


view : PrivateState -> Model -> (Title, Html Msg)
view state model = 
  let
    ( admin, _ ) = state.admin
    
    welcomeMsg = "Hello " ++ admin.username ++ "! Looks like you haven't registered any sites yet."

    submitBtn = 
      U.button "Submit"
      |> U.onClick (SubmitDomain model.hostname)

    loading =
      div [ class "loading-container" ]
        [ Loader.donut ]

    content =
      case state.sites of
        RemoteData.NotAsked -> loading
        RemoteData.Loading  -> loading
        RemoteData.Success sites -> viewSites sites
          
        -- TODO: refactor this branch of
        -- the case statement. I don't recall
        -- why this view is the view for the error
        -- case
        RemoteData.Failure err ->
          div []
            [ text welcomeMsg
            , input
                [ type_ "text"
                , class "site-input"
                , placeholder "enter a domain name ..."
                , onInput SiteInput
                , value model.hostname
                ] []
            , U.toHtml submitBtn
            ]
            
    viewWithNav = withVnav model ResponsiveNavMsg

    navigationOpts = 
      div [ class "font-bold" ]
        [ div [] [ cog, text "settings" ] 
        , button [ class "", onClick LogOut ] [ text "Log Out" ]
        ]

    html =
      viewWithNav
        navigationOpts
        (div [ class "" ]
          [ h1 [] [ text "Websites" ]
          , content
          , Toast.view ToastMsg model.toasties
          ])
  in 
  ( "Admin Panel", html )
