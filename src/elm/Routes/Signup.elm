module Routes.Signup exposing
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
import Api.Deserialize as Input
import Browser.Navigation as Nav
import Email
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
    , email : String
    , password : PasswordFieldValue
    , passwordConfirm : PasswordFieldValue
    }


type alias Model =
    { signupForm : FV.Model FormValues
    , toasts : Toast.ToastState
    }


type Msg
    = FormChanged (FV.Model FormValues)
    | SignUp String String String String
    | FormSubmitted (Result Http.Error Input.AdminWithToken)
    | ToastMsg Toast.ToastMsg


initModel : Model
initModel =
    let
        initialPasswordFieldValue =
            { value = ""
            , textVisible = False
            }

        formModel =
            FV.idle
                { username = ""
                , email = ""
                , password = initialPasswordFieldValue
                , passwordConfirm = initialPasswordFieldValue
                }
    in
    { signupForm = formModel
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

        emailField =
            Form.inputField
                { parser =
                    \rawEmail ->
                        if Email.isValid rawEmail then
                            Ok rawEmail

                        else
                            Err "Invalid email"
                , value = .email
                , update = \email values -> { values | email = email }
                , error = always Nothing
                , attributes =
                    { label = "Email"
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

        passwordConfirmField =
            Form.meta
                (\values ->
                    Form.passwordField
                        { parser =
                            \{ value } ->
                                if value == values.password.value then
                                    Ok value

                                else
                                    Err "The passwords do not match"
                        , value = .passwordConfirm
                        , update = \pswrd vals -> { vals | passwordConfirm = pswrd }
                        , error = always Nothing
                        , attributes =
                            { label = "Confirm Password"
                            , placeholder = ""
                            }
                        }
                )
    in
    Form.succeed SignUp
        |> Form.append usernameField
        |> Form.append emailField
        |> Form.append passwordField
        |> Form.append passwordConfirmField


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
    case msg of
        FormChanged newFormState ->
            ( { model | signupForm = newFormState }
            , Cmd.none
            , SharedState.NoUpdate
            )

        SignUp username email password passwordConfirm ->
            let
                api =
                    Utils.getApi state

                cmd =
                    Api.adminSignup api
                        FormSubmitted
                        { username = username
                        , email = email
                        , password = password
                        , passwordConfirm = passwordConfirm
                        }
            in
            ( model
            , cmd
            , SharedState.NoUpdate
            )

        FormSubmitted result ->
            case result of
                Ok ( admin, token ) ->
                    let
                        redirectCmd =
                            Nav.pushUrl (Utils.getNavKey state) "/dash"

                        setTokenCmd =
                            Utils.setToken token

                        sharedStateUpdate =
                            SharedState.SetAdmin ( admin, token )
                    in
                    ( model
                    , Cmd.batch [ redirectCmd, setTokenCmd ]
                    , sharedStateUpdate
                    )

                Err err ->
                    let
                        ( newModel, cmd ) =
                            ( model, Cmd.none )
                                |> Toast.addToast ToastMsg "Something Went Wrong"
                    in
                    ( newModel
                    , cmd
                    , SharedState.NoUpdate
                    )

        ToastMsg toastMsg ->
            let
                ( newModel, cmd ) =
                    Toast.update ToastMsg toastMsg model
            in
            ( newModel, cmd, SharedState.NoUpdate )


type alias Title =
    String


view : PublicState -> Model -> ( Title, Html Msg )
view _ model =
    let
        signupForm =
            FV.toHtml
                { onChange = FormChanged
                , action = "Sign Up"
                , loading = "loading ..."
                , validation = FV.ValidateOnSubmit
                }
                form
                model.signupForm

        signInLink =
            Text.text "log in instead"
                |> Text.withType (Text.Link "/login" Text.Self)
                |> Text.toHtml

        markup =
            div []
                [ div [ class "flex flex-col justify-center" ]
                    [ h1 [ class "mb-6 text-2xl text-gray-900" ] [ text "Sign Up" ]
                    , signupForm
                    , signInLink
                    ]
                , Toast.view ToastMsg model.toasts
                ]
    in
    ( "Signup", markup )
