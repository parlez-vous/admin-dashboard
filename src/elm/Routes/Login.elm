module Routes.Login exposing
    ( Model
    , Msg
    , initModel
    , update
    , view
    )

import Ant.Form as Form exposing (Form)
import Ant.Form.PasswordField exposing (PasswordFieldValue)
import Ant.Form.View as FV
import Ant.Typography.Text as Text
import Api
import Api.Deserialize exposing (AdminWithToken)
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Http
import SharedState exposing (PublicState, SharedState, SharedStateUpdate)
import UI.Link as Link
import UI.Nav exposing (withHnav)
import UI.Toast as Toast
import Utils


type alias FormValues =
    { username : String
    , password : PasswordFieldValue
    }


type alias Model =
    { loginForm : FV.Model FormValues
    , toasts : Toast.ToastState
    }


type Msg
    = FormChanged (FV.Model FormValues)
    | Login String String
    | FormSubmitted (Result Http.Error AdminWithToken)
    | ToastMsg Toast.ToastMsg


initModel : Model
initModel =
    let
        formModel =
            FV.idle
                { username = ""
                , password = { value = "", textVisible = False }
                }
    in
    { loginForm = formModel
    , toasts = Toast.init
    }


form : Form FormValues Msg
form =
    let
        usernameField =
            Form.inputField
                { parser = Ok
                , value = .username
                , update = \username values -> { values | username = username }
                , error = always Nothing
                , attributes =
                    { label = "Username"
                    , placeholder = ""
                    }
                }

        passwordField =
            Form.passwordField
                { parser = \{ value } -> Ok value
                , value = .password
                , update = \pswrd values -> { values | password = pswrd }
                , error = always Nothing
                , attributes =
                    { label = "Password"
                    , placeholder = ""
                    }
                }
    in
    Form.succeed Login
        |> Form.append usernameField
        |> Form.append passwordField


updateFormState : Model -> FV.State -> Model
updateFormState ({ loginForm } as model) newState =
    { model
        | loginForm = { loginForm | state = newState }
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
    case msg of
        FormChanged newFormModel ->
            ( { model | loginForm = newFormModel }
            , Cmd.none
            , SharedState.NoUpdate
            )

        Login username password ->
            let
                api =
                    Utils.getApi state

                cmd =
                    Api.adminSignin api
                        FormSubmitted
                        { username = username
                        , password = password
                        }
            in
            ( updateFormState model FV.Loading
            , cmd
            , SharedState.NoUpdate
            )

        FormSubmitted result ->
            case result of
                Ok adminWithToken ->
                    let
                        navKey =
                            Utils.getNavKey state

                        commands =
                            Cmd.batch
                                [ Utils.setToken <| Tuple.second adminWithToken
                                , Nav.pushUrl navKey "/dash"
                                ]
                    in
                    ( model
                    , commands
                    , SharedState.SetAdmin adminWithToken
                    )

                Err e ->
                    let
                        ( newModel, cmd ) =
                            ( model, Cmd.none )
                                |> Toast.addToast ToastMsg "Something Went Wrong"
                    in
                    ( updateFormState newModel FV.Idle
                    , cmd
                    , SharedState.NoUpdate
                    )

        ToastMsg toastMsg ->
            let
                ( newModel, cmd ) =
                    Toast.update ToastMsg toastMsg model
            in
            ( newModel, cmd, SharedState.NoUpdate )



-- View


type alias Title =
    String


view : PublicState -> Model -> ( Title, Html Msg )
view _ model =
    let
        loginForm =
            FV.toHtml
                { onChange = FormChanged
                , action = "Log In"
                , loading = "loading ..."
                , validation = FV.ValidateOnSubmit
                }
                form
                model.loginForm

        signUpLink =
            Text.text "sign up instead"
                |> Text.withType (Text.Link "/signup" Text.Self)
                |> Text.toHtml

        markup =
            div []
                [ div [ class "flex flex-col justify-center" ]
                    [ h1 [ class "mb-6 text-2xl text-gray-900" ] [ text "Log Into Parlez Vous" ]
                    , loginForm
                    , signUpLink
                    ]
                , Toast.view ToastMsg model.toasts
                ]
    in
    ( "Login", markup )
