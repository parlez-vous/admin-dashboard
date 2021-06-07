module Routes.RegisterSite exposing
    ( Model
    , Msg
    , initModel
    , update
    , view
    )

import Ant.Form as Form exposing (Form)
import Ant.Form.View as FV
import Api
import Api.Deserialize as Input
import Api.Output as Output
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import SharedState exposing (PrivateState, SharedStateUpdate(..))
import UI.Link exposing (externalLink)
import UI.Nav as ResponsiveNav exposing (withVnav)
import UI.Toast as Toast
import Url exposing (Url)


type alias FormValues =
    { domain : String
    }


type alias Model =
    { registerSiteForm : FV.Model FormValues
    , toasts : Toast.ToastState
    , navbar : ResponsiveNav.NavState
    }


type Msg
    = FormChanged (FV.Model FormValues)
    | RegisterSite Url
    | FormSubmitted (Result Http.Error Input.Site)
    | ToastMsg Toast.ToastMsg
    | ResponsiveNavMsg ResponsiveNav.Msg


initModel : Model
initModel =
    let
        formModel =
            FV.idle
                { domain = ""
                }
    in
    { registerSiteForm = formModel
    , toasts = Toast.init
    , navbar = ResponsiveNav.init
    }


addToast : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast =
    Toast.addToast ToastMsg


form : Form FormValues Msg
form =
    let
        domainInputField =
            Form.inputField
                { parser =
                    \rawInput ->
                        let
                            withProtocol =
                                "https://" ++ rawInput
                        in
                        case Url.fromString withProtocol of
                            Nothing ->
                                Err "Invalid URL"

                            Just url ->
                                -- Naive check to see if the URL is a FQDN
                                if String.contains "." url.host then
                                    Ok url

                                else
                                    Err "The Url must be a fully qualified domain name"
                , value = .domain
                , update = \value values -> { values | domain = value }
                , error = always Nothing
                , attributes =
                    { label = "Domain Name"
                    , placeholder = "example.com"
                    }
                }
    in
    Form.succeed RegisterSite
        |> Form.append domainInputField


update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
    case msg of
        FormChanged newForm ->
            ( { model
                | registerSiteForm = newForm
              }
            , Cmd.none
            , NoUpdate
            )

        RegisterSite url ->
            let
                data =
                    Output.RegisterSite url.host

                ( _, token ) =
                    state.admin

                { registerSite } =
                    Api.getApiClient state.api
            in
            ( model
            , registerSite token FormSubmitted data
            , NoUpdate
            )

        FormSubmitted result ->
            case result of
                Ok site ->
                    ( model
                    , UI.Link.toHref (UI.Link.Site site.id) |> Nav.pushUrl state.navKey
                    , SharedState.InsertSite site
                    )

                Err e ->
                    let
                        ( newModel, cmd ) =
                            case e of
                                Http.BadStatus statusCode ->
                                    if statusCode == 400 then
                                        ( model, Cmd.none )
                                            |> addToast "Make sure you enter a Fully Qualified Domain Name!"

                                    else if statusCode == 409 then
                                        ( model, Cmd.none )
                                            |> addToast "This Site is already registered!"

                                    else
                                        ( model, Cmd.none )
                                            |> addToast "Something went wrong"

                                _ ->
                                    ( model, Cmd.none )
                                        |> addToast "Something went wrong"
                    in
                    ( newModel, cmd, NoUpdate )

        ToastMsg subMsg ->
            let
                ( m, cmd ) =
                    model
                        |> Toast.update ToastMsg subMsg
            in
            ( m
            , cmd
            , NoUpdate
            )

        ResponsiveNavMsg subMsg ->
            ResponsiveNav.update subMsg model


type alias Title =
    String


view : ( a, PrivateState ) -> Model -> ( Title, Html Msg )
view ( _, state ) model =
    let
        viewWithNav =
            withVnav state model ResponsiveNavMsg

        domainInputForm =
            FV.toHtml
                { onChange = FormChanged
                , action = "Submit"
                , loading = "loading ..."
                , validation = FV.ValidateOnSubmit
                }
                form
                model.registerSiteForm

        content =
            div []
                [ h1 [] [ text "Register a domain" ]
                , text "Ensure that the domain you enter is a "
                , externalLink "https://en.wikipedia.org/wiki/Fully_qualified_domain_name" "fully-qualified domain name"
                , domainInputForm
                ]

        html =
            viewWithNav
                (div [ class "my-5 mx-8" ]
                    [ content
                    , Toast.view ToastMsg model.toasts
                    ]
                )
    in
    ( "Register Site", html )
