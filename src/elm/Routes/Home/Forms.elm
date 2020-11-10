module Routes.Home.Forms exposing
    ( FormSubmittedResult
    , LogInFormModel
    , LogInFormValues
    , SignUpFormModel
    , SignUpFormValues
    , handleFormSubmitted
    , handleSubmitLogin
    , handleSubmitSignup
    , initialLogInFormModel
    , initialSignUpFormModel
    , logInForm
    , signUpForm
    )

import Ant.Form as Form exposing (Form)
import Ant.Form.PasswordField exposing (PasswordFieldValue)
import Ant.Form.View as FV
import Api
import Api.Deserialize exposing (AdminWithToken)
import Browser.Navigation as Nav
import Email
import Http
import SharedState exposing (SharedState, SharedStateUpdate)
import Utils


{-| Refactoring ideas
type alias ApiClient =
{ adminSignIn
, adminSignup
, getSites
}

createApi : SharedState -> ApiClient

-}
type alias FormSubmittedResult =
    Result Http.Error AdminWithToken


type alias SignUpFormModel =
    FV.Model SignUpFormValues


type alias SignUpFormValues =
    { username : String
    , email : String
    , password : PasswordFieldValue
    , passwordConfirm : PasswordFieldValue
    }


type alias LogInFormModel =
    FV.Model LogInFormValues


type alias LogInFormValues =
    { username : String
    , password : PasswordFieldValue
    }


{-| Initiates API request to server
-}
handleSubmitLogin :
    SharedState
    -> (FormSubmittedResult -> msg)
    -> LogInFormModel
    -> { username : String, password : String } --> 'a' ???
    -> ( LogInFormModel, Cmd msg )
handleSubmitLogin state tagger formModel data =
    let
        api =
            Utils.getApi state

        cmd =
            Api.adminSignin api tagger data
    in
    ( { formModel | state = FV.Loading }
    , cmd
    )


handleSubmitSignup :
    SharedState
    -> (FormSubmittedResult -> msg)
    -> SignUpFormModel
    -> { email : String, username : String, password : String, passwordConfirm : String }
    -> ( SignUpFormModel, Cmd msg )
handleSubmitSignup state tagger formModel data =
    let
        api =
            Utils.getApi state

        cmd =
            Api.adminSignup api tagger data
    in
    ( { formModel | state = FV.Loading }
    , cmd
    )


{-| Handles server response
-}
handleFormSubmitted : FormSubmittedResult -> SharedState -> ( Cmd msg, SharedStateUpdate )
handleFormSubmitted result state =
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
            ( commands
            , SharedState.SetAdmin adminWithToken
            )

        Err e ->
            let
                _ =
                    Debug.log "[handleFormSubmitted] Error - " e
            in
            ( Cmd.none
            , SharedState.NoUpdate
            )



{-
   updateFormState : Model -> FV.State -> Model
   updateFormState ({ loginForm } as model) newState =
       { model
           | loginForm = { loginForm | state = newState }
       }
-}


initialSignUpFormModel : SignUpFormModel
initialSignUpFormModel =
    let
        initialPasswordFieldValue =
            { value = ""
            , textVisible = False
            }
    in
    FV.idle
        { username = ""
        , email = ""
        , password = initialPasswordFieldValue
        , passwordConfirm = initialPasswordFieldValue
        }


signUpForm :
    (String -> String -> String -> String -> msg)
    -> Form SignUpFormValues msg
signUpForm tagger =
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
    Form.succeed tagger
        |> Form.append usernameField
        |> Form.append emailField
        |> Form.append passwordField
        |> Form.append passwordConfirmField


initialLogInFormModel : LogInFormModel
initialLogInFormModel =
    FV.idle
        { username = ""
        , password = { value = "", textVisible = False }
        }


logInForm : (String -> String -> msg) -> Form LogInFormValues msg
logInForm tagger =
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
    Form.succeed tagger
        |> Form.append usernameField
        |> Form.append passwordField
