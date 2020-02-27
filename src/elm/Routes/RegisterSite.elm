module Routes.RegisterSite exposing
  ( Model
  , Msg
  , initModel
  , update
  , view
  )

import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import SharedState exposing (PrivateState, SharedStateUpdate(..))

import Api
import Api.Deserialize as Input
import Api.Output as Output
import UI.Button as Btn
import UI.Input as Input
import UI.Link exposing (externalLink)
import UI.Toast as Toast
import UI.Nav as ResponsiveNav exposing (withVnav)




type alias Model =
  { hostname : String
  , toasts : Toast.ToastState
  , navbar   : ResponsiveNav.NavState
  }

type Msg
  = DomainInput String
  | SubmitDomain String  
  | DomainSubmitted (Result Http.Error Input.Site)
  | ToastMsg Toast.ToastMsg
  | ResponsiveNavMsg ResponsiveNav.Msg
  


initModel : Model
initModel =
  { hostname = ""
  , toasts = Toast.init 
  , navbar = ResponsiveNav.init
  }


addToast : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast = Toast.addToast ToastMsg


update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case msg of
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
                      |>  addToast "Make sure you enter a Fully Qualified Domain Name!" 
                  else if statusCode == 409 then
                    ( model, Cmd.none )
                      |> addToast "This Site is already registered!"
                  else
                    ( model, Cmd.none )
                    |> addToast "Something went wrong"

                _ -> ( model, Cmd.none )
                  |> addToast "Something went wrong"
          in
            ( newModel, cmd, NoUpdate )
    
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


type alias Title = String

view : PrivateState -> Model -> (Title, Html Msg)
view state model =
  let
    viewWithNav = withVnav state model ResponsiveNavMsg

    content =
      div []
        [ h1 [] [ text "Register a domain" ]
        , text "Ensure that the domain you enter is a "
        , externalLink "https://en.wikipedia.org/wiki/Fully_qualified_domain_name" "fully-qualified domain name"
        , Input.input (Input.Url model.hostname DomainInput)
          |> Input.toHtml
        , Btn.button "submit"
          |> Btn.onClick (SubmitDomain model.hostname)
          |> Btn.toHtml
        ]

    html =
      viewWithNav
        (div [ class "my-5 mx-8" ]
          [ content
          , Toast.view ToastMsg model.toasts
          ])
  in
  
  ( "Register Site", html )
  