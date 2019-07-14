module Routes.Admin exposing
  ( Model
  , Msg
  , init
  , initCmd
  , update
  , view
  )

import Browser.Navigation as Nav
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
import Session
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Utils exposing (logout)
import UI.Icons exposing (bell, logo, user, cog)
import UI.Button as U
import UI.Loader as Loader
import UI.Toast as Toast


-- MODEL

type alias Sites = List Input.Site

type alias Model =
  { hostname : String
  , toasties : Toast.ToastState
  , sites : WebData Sites
  }


type Msg
  = LogOut
  | SiteInput String
  | SubmitDomain String
  | DomainSubmitted (Result Http.Error Input.Site)
  | ToastMsg (Toasty.Msg String)
  | SitesResponse (WebData Sites)



init : Model
init =
  { hostname = ""
  , toasties = Toast.init
  , sites = RemoteData.NotAsked
  }


initCmd : Input.SessionToken -> String -> Cmd Msg
initCmd token api = Api.getSites token api SitesResponse


isValidHostname : String -> Bool
isValidHostname rawDomain =
  let 
    withProtocol = "https://" ++ rawDomain

  in
    case Url.fromString withProtocol of
      Nothing -> False
      Just _ -> True


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case (state.session, msg) of

    -- TODO: refactor update fn for private routes
    -- Context: a session of Session.Guest should never occur
    -- here since we manage redirection on private routes
    -- at src/elm/Routes/Router.elm
    ( Session.Guest, _ ) ->
      ( model
      , Nav.pushUrl state.navKey "/"
      , NoUpdate
      )

    ( Session.Admin _, LogOut ) ->
      let
        ( logOutCmd, sharedStateUpdate ) = logout

      in
        ( model
        , Cmd.batch [ logOutCmd, Nav.pushUrl state.navKey "/" ]
        , sharedStateUpdate
        )

    ( Session.Admin _, SiteInput rawDomain ) ->
      ( { model | hostname = rawDomain
        }
      , Cmd.none
      , NoUpdate
      )
        
    ( Session.Admin ( admin, token ), SubmitDomain rawDomain ) ->
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
        in
          ( model
          , Api.registerSite token state.api DomainSubmitted data
          , NoUpdate 
          )

    ( _, DomainSubmitted result ) ->
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
    ( _, ToastMsg subMsg ) ->
      let
        ( m , cmd ) =
          model
          |> Toasty.update Toast.config ToastMsg subMsg

      in
        ( m
        , cmd
        , NoUpdate
        )
    
    ( _, SitesResponse response ) ->
      ( { model | sites = response}
      , Cmd.none
      , NoUpdate
      )
      
      





type alias Title = String

viewSite : Input.Site -> Html Msg
viewSite site =
  let
    link = "/sites/" ++ String.fromInt site.id

    status = if site.verified
      then "Verified"
      else "Not Verified :("

  in
    div [ class "site-container" ]
      [ header [ class "site-details" ]
          [ a [ href link ] [ h2 [] [ text site.hostname ] ]
          
          -- This is a placeholder for what I imagine
          -- to be site details
          , p [] [ text "No data to display" ]
          ]
      , div [ class "site-status" ]
          [ text status ]
      ]


view : SharedState -> Input.Admin -> Model -> (Title, Html Msg)
view sharedState admin model = 
  let
    welcomeMsg = "Hello " ++ admin.username ++ "! Looks like you haven't registered any sites yet."

    submitBtn = 
      U.button (SubmitDomain model.hostname) "Submit"

    loading =
      div [ class "loading-container" ]
        [ Loader.donut ]

    content =
      case model.sites of
        RemoteData.NotAsked -> loading
        RemoteData.Loading  -> loading
        RemoteData.Success sites ->
          div []
            [ text <| "You have " ++ (String.fromInt <| List.length sites) ++ " sites!"
            , div [] <| List.map viewSite sites
            ]

          
          
        _ ->
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
            

    html =
      div [ class "admin-page vertical-nav-container" ]

        -- Vertical Nav
        [ nav [ class "vertical-navbar" ]
            [ div [ class "logo-container" ] [ logo "50" ]
            , div [ class "nav-primary-content"] [  ]
            
            , div [ class "nav-secondary-content" ]
                [ bell
                , div [ class "user-container" ] [ user ] 
                , cog
                ]
            ]

        -- Content
        , div [ class "content" ]
          [ h1 [] [ text "Websites" ]
          , content
          , button [ class "logout", onClick LogOut ] [ text "Log Out" ]
          ]

        , Toast.view ToastMsg model.toasties
        ]

  in 
  ( "Admin Panel", html )
