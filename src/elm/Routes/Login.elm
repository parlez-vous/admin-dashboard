module Routes.Login exposing
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
import SharedState exposing (SharedState, PublicState, SharedStateUpdate)
import UI.Nav exposing (hnav)
import UI.Button as Btn exposing (button, link)
import UI.Toast as Toast
import Utils


type alias Model = 
  { username : String
  , password : String

  -- maybe rename to `locked`?
  , formSubmitting : Bool

  , toasties : Toast.ToastState
  }


type Msg
  = UpdateUsername String
  | UpdatePassword String
  | SubmitForm
  | FormSubmitted (Result Http.Error Input.AdminWithToken)
  | ToastMsg (Toasty.Msg String)


initModel : Model
initModel =
  { username = ""
  , password = ""
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

    SubmitForm ->
      let
        api = Utils.getApi state

        newModel = { model | formSubmitting = True }

        cmd = Api.adminSignin api FormSubmitted model
      in
        ( newModel
        , cmd
        , SharedState.NoUpdate
        )

    FormSubmitted result ->
      case result of
        Ok adminWithToken -> 
          let
            navKey = Utils.getNavKey state

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
            toastMsg = "Something Went Wrong"

            ( newModel, cmd ) = ( model, Cmd.none )
              |> Toasty.addToast Toast.config ToastMsg toastMsg
          in
            (newModel, cmd, SharedState.NoUpdate)

    ToastMsg toastMsg ->
      let
        ( newModel, cmd ) = model
          |> Toasty.update Toast.config ToastMsg toastMsg
      in
        ( newModel, cmd, SharedState.NoUpdate )



-- View

type alias Title = String


loginForm : Model -> Html Msg
loginForm model =
  let
    classes = Utils.toClass
      [ "border"
      , "border-solid"
      , "border-gray-400"
      , "rounded"
      , "px-4"
      , "py-2"
      , "w-64"
      , "block"
      , "bg-gray-200"
      , "focus:bg-gray-100"
      , "focus:outline-none"
      , "text-gray-700"
      , "leading-tight"
      ]

  in
  H.form [ class "md:w-1/2 mx-auto", onSubmit SubmitForm ]
    [ div [ class "md:flex md:items-center" ]
        [ H.label [ A.for "username", class "w-5/12 md:pr-6 text-gray-500 font-semibold" ] [ text "Username / Email" ]
        , H.input
            [ A.type_ "input"
            , A.placeholder "username..."
            , A.id "username"
            , A.autocomplete False
            , onInput UpdateUsername
            , value model.username
            , classes
            ]
            []
        ]
    , div [ class "md:flex md:items-center mt-4" ]
        [ H.label [ A.for "password", class "w-5/12 md:pr-6 text-gray-500 font-semibold" ] [ text "Password" ]
        , H.input
            [ A.type_ "password"
            , A.placeholder "password..."
            , A.id "password"
            , onInput UpdatePassword
            , value model.password
            , classes
            ]
            []
        ]
    , div [ class "flex justify-center mt-6" ]
        [ Btn.toHtml <| button "Log In"
        , link Btn.Signup "Sign Up"
          |> Btn.secondary
          |> Btn.toHtml
        ]
    ]




view : PublicState -> Model -> (Title, Html Msg)
view _ model =
  let
    markup =
      div []
        [ hnav [ Btn.toHtml <| link Btn.Signup "sign up" ]
        , div [ class "mx-auto md:mx-64" ]
            [ h1 [ class "text-center mb-6 text-2xl text-gray-900" ] [ text "Log Into Parlez Vous" ]
            , loginForm model
            ]
        , Toast.view ToastMsg model.toasties
        ]

  in
  ( "Login", markup )
