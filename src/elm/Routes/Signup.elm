module Routes.Signup exposing
  ( Model
  , Msg
  , initModel
  , view
  , update
  )

import Browser.Navigation as Nav
import Html as H exposing (div, text, h1, Html)
import Html.Attributes as A exposing (class, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Toasty


import Api
import Api.Deserialize as Input
import UI.Nav exposing (withHnav)
import UI.Button as Btn exposing (button, link)
import UI.Toast as Toast
import UI.Input as Input
import Utils
  

import SharedState exposing (SharedState, PublicState, SharedStateUpdate)



type alias Model = 
  { username : String
  , password : String
  , passwordConfirm : String

  -- maybe rename to `locked`?
  , formSubmitting : Bool

  , toasties : Toast.ToastState
  }



type Msg
  = UpdateUsername String
  | UpdatePassword String
  | UpdatePassConfirm String
  | SubmitForm
  | FormSubmitted (Result Http.Error Input.AdminWithToken)
  | ToastMsg (Toasty.Msg String)



initModel : Model
initModel =
  { username = ""
  , password = ""
  , passwordConfirm = ""
  , formSubmitting = False
  , toasties = Toast.init
  }




update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case msg of
    UpdateUsername username ->
      ( { model | username = username }
      , Cmd.none
      , SharedState.NoUpdate
      )

    UpdatePassword password ->
      ( { model | password = password }
      , Cmd.none
      , SharedState.NoUpdate
      )

    UpdatePassConfirm passConfirm ->
      ( { model | passwordConfirm = passConfirm }
      , Cmd.none
      , SharedState.NoUpdate
      )

    SubmitForm ->
      let
        api = Utils.getApi state

        newModel = { model | formSubmitting = True }

        cmd = Api.adminSignup api FormSubmitted model
      in
        ( newModel
        , cmd
        , SharedState.NoUpdate
        )

    FormSubmitted result ->
      case result of
        Ok (admin, token) ->
          let
            redirectCmd = Nav.pushUrl (Utils.getNavKey state) "/dash"
            setTokenCmd = Utils.setToken token

            sharedStateUpdate = SharedState.SetAdmin (admin, token)
          in
          ( model
          , Cmd.batch [ redirectCmd, setTokenCmd ]
          , sharedStateUpdate
          )

        Err err ->
          let
            toastMsg = "Something Went Wrong"
            
            ( newModel, cmd ) = ( model, Cmd.none )
              |> Toasty.addToast Toast.config ToastMsg toastMsg
          in
          ( newModel
          , cmd
          , SharedState.NoUpdate
          )


    ToastMsg toastMsg ->
      let
        ( newModel, cmd ) = model
          |> Toasty.update Toast.config ToastMsg toastMsg
      in
        ( newModel, cmd, SharedState.NoUpdate )





loginForm : Model -> Html Msg
loginForm model =
  let
    usernameInput = Input.input (Input.Text model.username UpdateUsername)
      |> Input.withNoAutocomplete
      |> Input.withPlaceholder "username..."
      |> Input.withLabel "Username" "username-input"
      |> Input.toHtml

    passwordInput = Input.input (Input.Password model.password UpdatePassword)
      |> Input.withPlaceholder "password"
      |> Input.withLabel "Password" "password-input"
      |> Input.toHtml

    
    pswdConfirmInput = Input.input (Input.Password model.passwordConfirm UpdatePassConfirm)
      |> Input.withPlaceholder "confirm password ..."
      |> Input.withLabel "Confirm Password" "password-confirm-input"
      |> Input.toHtml

  in
  H.form [ class "mx-auto", onSubmit SubmitForm ]
    [ usernameInput
    , passwordInput
    , pswdConfirmInput
    , div [ class "flex justify-center mt-6" ]
        [ Btn.toHtml <| button "Sign Up"
        , link Btn.Login "Log In"
          |> Btn.secondary
          |> Btn.toHtml
        ]
    ]




type alias Title = String

view : PublicState -> Model -> (Title, Html Msg)
view _ model =
  let
    markup =
      withHnav
        [ Btn.toHtml <| link Btn.Signup "sign up" ]
        [ div [ class "flex flex-col justify-center" ]
            [ h1 [ class "mb-6 text-2xl text-gray-900" ] [ text "Sign Up" ]
            , loginForm model
            ]
        , Toast.view ToastMsg model.toasties
        ]

  in
  ( "Signup", markup )  
