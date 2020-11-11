module Routes.Home exposing
    ( Model
    , Msg
    , initModel
    , update
    , view
    )

import Ant.Button as Btn
import Ant.Form.View as FV
import Ant.Layout as Layout
import Ant.Typography as T
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Routes.Home.Forms as Forms exposing (..)
import SharedState exposing (SharedState(..), SharedStateUpdate)
import Utils



-- MSG


type Msg
    = LogInFormChanged LogInFormModel
    | SignUpFormChanged SignUpFormModel
    | ActiveFormToggled
    | LogIn String String
    | SignUp String String String String
    | FormSubmitted FormSubmittedResult


type ActiveForm
    = LogInActive
    | SignUpActive


type alias Model =
    { logInForm : LogInFormModel
    , signUpForm : SignUpFormModel
    , activeForm : ActiveForm
    }


initModel : Model
initModel =
    { logInForm = initialLogInFormModel
    , signUpForm = initialSignUpFormModel
    , activeForm = SignUpActive
    }



-- UPDATE


{-| Does no-ops for Cmd and ShareStateUpdate
-}
simpleUpdate : Model -> ( Model, Cmd Msg, SharedStateUpdate )
simpleUpdate newModel =
    ( newModel, Cmd.none, SharedState.NoUpdate )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        LogInFormChanged newFormState ->
            simpleUpdate { model | logInForm = newFormState }

        SignUpFormChanged newFormState ->
            simpleUpdate { model | signUpForm = newFormState }

        ActiveFormToggled ->
            let
                newActiveForm =
                    case model.activeForm of
                        LogInActive ->
                            SignUpActive

                        SignUpActive ->
                            LogInActive
            in
            simpleUpdate { model | activeForm = newActiveForm }

        LogIn username password ->
            let
                api =
                    Utils.getApi sharedState

                ( newFormModel, cmd ) =
                    Forms.handleSubmitLogin
                        api
                        FormSubmitted
                        model.logInForm
                        { username = username
                        , password = password
                        }
            in
            ( { model | logInForm = newFormModel }
            , cmd
            , SharedState.NoUpdate
            )

        SignUp username email password passwordConfirm ->
            let
                api =
                    Utils.getApi sharedState

                ( newFormModel, cmd ) =
                    Forms.handleSubmitSignup
                        api
                        FormSubmitted
                        model.signUpForm
                        { username = username
                        , password = password
                        , email = email
                        , passwordConfirm = passwordConfirm
                        }
            in
            ( { model | signUpForm = newFormModel }
            , cmd
            , SharedState.NoUpdate
            )

        FormSubmitted submissionResult ->
            let
                ( cmd, sharedStateUpdate ) =
                    Forms.handleFormSubmitted submissionResult sharedState
            in
            ( model, cmd, sharedStateUpdate )



-- View


type alias Title =
    String


view : SharedState -> Model -> ( Title, Html Msg )
view _ model =
    let
        logInForm =
            FV.toHtml
                { onChange = LogInFormChanged
                , action = "Log In"
                , loading = "loading ..."
                , validation = FV.ValidateOnSubmit
                }
                (Forms.logInForm LogIn)
                model.logInForm

        signUpForm =
            FV.toHtml
                { onChange = SignUpFormChanged
                , action = "Sign Up"
                , loading = "loading ..."
                , validation = FV.ValidateOnSubmit
                }
                (Forms.signUpForm SignUp)
                model.signUpForm

        activeForm =
            case model.activeForm of
                SignUpActive ->
                    signUpForm

                LogInActive ->
                    logInForm

        headerContent =
            div [] [ text "logo placeholder" ]

        toggleActiveFormButton =
            let
                buttonLabel =
                    case model.activeForm of
                        SignUpActive ->
                            "or log in"

                        LogInActive ->
                            "or sign up"
            in
            Btn.button buttonLabel
                |> Btn.onClick ActiveFormToggled
                |> Btn.withType Btn.Link
                |> Btn.toHtml

        heading =
            let
                headingContent =
                    case model.activeForm of
                        SignUpActive ->
                            "Sign Up To ParlezVous"

                        LogInActive ->
                            "Log In To ParlezVous"
            in
            T.title headingContent
                |> T.level T.H2
                |> T.toHtml

        mainContent =
            div []
                [ heading
                , activeForm
                , toggleActiveFormButton
                ]

        layoutContent =
            Layout.layout2
                (Layout.header headerContent)
                (Layout.content mainContent)
    in
    ( "Home", Layout.toHtml layoutContent )
