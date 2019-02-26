module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import List


import Logo exposing (logo)




main =
  Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none





-- MODEL

type FormMsg
  = UsernameInput String
  | PasswordInput String
  | PasswordConfirm String


type Msg
  = DisplayLogin
  | DisplaySignup
  | Form FormMsg
  | SubmitForm
  | SubmittedForm (Result Http.Error ())



type alias Signup = 
  { username           : String
  , password        : String
  , passwordConfirm : String
  }


type alias Signin =
  { username    : String
  , password : String
  }

type Form
  = SignupForm Signup
  | SigninForm Signin

type Model
  = ShowForm Form
  | FormHidden
  | FormSubmitting


-- type alias Model = Int

init : () -> (Model, Cmd msg)
init _ = (FormHidden, Cmd.none)


-- UPDATE


updateFormField : Form -> FormMsg -> Form
updateFormField currentForm msg =
  case currentForm of
    SignupForm data ->
      case msg of
        UsernameInput s ->
          SignupForm { data | username = s }

        PasswordInput s ->
          SignupForm { data | password = s }

        PasswordConfirm s ->
          SignupForm { data | passwordConfirm = s }

    SigninForm data ->
      case msg of
        UsernameInput s ->
          SigninForm { data | username = s }

        PasswordInput s ->
          SigninForm { data | password = s }


        _ ->
          currentForm


signupJson : Signup -> E.Value
signupJson data =
  E.object
    [ ( "username", E.string data.username )
    , ( "password", E.string data.password )
    , ( "passwordConfirm", E.string data.passwordConfirm )
    ]


signinJson : Signin -> E.Value
signinJson data =
  E.object
    [ ( "username", E.string data.username )
    , ( "password", E.string data.password )
    ]


handleSubmitForm : Form -> Cmd Msg
handleSubmitForm form =
  let
    api = "http://staging.api.parlez-vous.io/admins"

  in
    case form of 
      SignupForm data ->
        Http.post
            { url = api ++ "/signup"
            , body = Http.jsonBody <| signupJson data
            , expect = Http.expectWhatever SubmittedForm
            }

      SigninForm data ->
        Http.post
          { url = api ++ "/signin"
          , body = Http.jsonBody <| signinJson data
          , expect = Http.expectWhatever SubmittedForm
          }



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DisplayLogin ->
      let emptyForm = SigninForm <| Signin "" ""
      in
        (ShowForm emptyForm, Cmd.none)

    DisplaySignup ->
      let emptyForm = SignupForm <| Signup "" "" ""
      in
        (ShowForm emptyForm, Cmd.none)


    Form formMsg ->
      let
        updatedModel =
          case model of
            ShowForm form ->
              ShowForm <| updateFormField form formMsg
          
            -- this seems like a code smell
            -- this state should never occur
            _ -> model

      in
        (updatedModel, Cmd.none)

    SubmitForm ->
      let httpCmd =
            case model of
              ShowForm form ->
                handleSubmitForm form
              
              -- this seems like a code smell
              -- this state should never occur
              _ -> Cmd.none

      in
        ( FormSubmitting
        , httpCmd
        )


    SubmittedForm result ->
      case result of
        Ok _ ->
          (Debug.log "success!" model, Cmd.none)

        Err _ ->
          (Debug.log "failure!" model, Cmd.none)




-- VIEW

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

view : Model -> Document Msg
view model =
  { title = "Parlez Vous"
  , body = [ body model ]
  }

body : Model -> Html Msg
body model =
  div [] 
    [ cta
    , div [ class "container" ]
      [ div [ class "row" ]
          [ h1 [ class "center-text slogan" ] [ text "Enable Conversations"]
          , pre [ class "center-text" ] [ text "work in progress" ]
          , div [ class "logo-container" ] [ logo ]
          , p [ class "center-text" ] [ text "The fastest way to engage your audience" ]
          , form_ model
          ]
      ]
    ]

cta : Html Msg
cta =
  div [ class "cta" ]
    [ button [ onClick DisplayLogin ] [ text "log in" ]
    , button [ class "button-primary", onClick DisplaySignup ]
        [ text "sign up"]
    ]


inputWithLabel : String -> String -> String -> String -> (String -> FormMsg) -> List (Html Msg)
inputWithLabel identifier t l v m =
  [ label [ for identifier ] [ text l ]
  , input [ id identifier, type_ t, onInput (Form << m), value v] []
  ]

usernameInput : String -> String -> (String -> FormMsg) -> List (Html Msg)
usernameInput identifier val msg =
  inputWithLabel identifier "text" "username" val msg

pswdInput : String -> String -> (String -> FormMsg) -> List (Html Msg)
pswdInput identifier val msg =
  inputWithLabel identifier "password" "password" val msg

form_ : Model -> Html Msg
form_ model =
  let
    readyToSubmit =
      case model of
        ShowForm form ->
          case form of
            SignupForm data -> 
              String.length data.username > 3 &&
              String.length data.password > 6 &&
              data.password == data.passwordConfirm

            SigninForm data ->
              String.length data.username > 3 &&
              String.length data.password > 6

        _ -> False

    submitBtn =
      if readyToSubmit
      then
        button [ ] [ text "submit" ]
      else
        div [] []
      

    baseForm { username, password } =
      List.append
        (usernameInput "username-input" username UsernameInput)
        (pswdInput "password-input" password PasswordInput)

    formContent =
      case model of
        FormHidden -> []

        FormSubmitting -> [ div [ class "center-text" ] [ text "submitting ..." ] ]

        ShowForm form ->
          case form of
            SignupForm data ->
              List.append (baseForm data)
                [ label [ for "password-confirm-input" ] [ text "confirm password" ]
                , input [ id "password-confirm-input", type_ "password", onInput (Form << PasswordConfirm) ] [] 
                , submitBtn
                ]

            SigninForm data ->
              List.append (baseForm data) [ submitBtn ]
          
  in
    Html.form [ class "custom-form", onSubmit SubmitForm ] formContent
