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
  = EmailInput String
  | PasswordInput String
  | PasswordConfirm String
  | WebsiteInput String


type Msg
  = DisplayLogin
  | DisplaySignup
  | Form FormMsg
  | SubmitForm
  | SubmittedForm (Result Http.Error ())



type alias Signup = 
  { email           : String
  , website         : String
  , password        : String
  , passwordConfirm : String
  }


type alias Signin =
  { email    : String
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
        EmailInput s ->
          SignupForm { data | email = s }

        PasswordInput s ->
          SignupForm { data | password = s }

        PasswordConfirm s ->
          SignupForm { data | passwordConfirm = s }

        WebsiteInput s ->
          SignupForm { data | website = s }

    SigninForm data ->
      case msg of
        EmailInput s ->
          SigninForm { data | email = s }

        PasswordInput s ->
          SigninForm { data | password = s }


        _ ->
          currentForm


signupJson : Signup -> E.Value
signupJson data =
  E.object
    [ ( "email", E.string data.email )
    , ( "website", E.string data.website )
    , ( "password", E.string data.password )
    , ( "passwordConfirm", E.string data.passwordConfirm )
    ]


signinJson : Signin -> E.Value
signinJson data =
  E.object
    [ ( "email", E.string data.email )
    , ( "password", E.string data.password )
    ]


handleSubmitForm : Form -> Cmd Msg
handleSubmitForm form =
  let
    api = "https://staging.api.parlez-vous.io"

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
      let emptyForm = SignupForm <| Signup "" "" "" ""
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


inputWithLabel : String -> String -> String -> (String -> FormMsg) -> List (Html Msg)
inputWithLabel identifier t v m =
  [ label [ for identifier ] [ text t ]
  , input [ id identifier, type_ t, onInput (Form << m), value v] []
  ]

emailInput : String -> String -> (String -> FormMsg) -> List (Html Msg)
emailInput identifier val msg =
  inputWithLabel identifier "email" val msg

pswdInput : String -> String -> (String -> FormMsg) -> List (Html Msg)
pswdInput identifier val msg =
  inputWithLabel identifier "password" val msg

form_ : Model -> Html Msg
form_ model =
  let
    readyToSubmit =
      case model of
        ShowForm form ->
          case form of
            SignupForm data -> 
              String.length data.email > 3 &&
              String.length data.website > 11 &&
              String.left 8 data.website == "https://" &&
              String.length data.password > 6 &&
              data.password == data.passwordConfirm

            SigninForm data ->
              String.length data.email > 3 &&
              String.length data.password > 6

        _ -> False

    submitBtn =
      if readyToSubmit
      then
        button [ ] [ text "submit" ]
      else
        div [] []
      

    baseForm { email, password } =
      List.append
        (emailInput "email-input" email EmailInput)
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

                , label [ for "website-input" ] [ text "your website" ]
                , div [ class "url-info" ] 
                    [ span [] [ text "must start with " ] 
                    , pre [] [ text "https://" ]
                    ]
                , input
                    [ id "website-input"
                    , type_ "url"
                    , pattern "https://.*"
                    , onInput (Form << WebsiteInput)
                    ]
                    []

                , submitBtn
                ]

            SigninForm data ->
              List.append (baseForm data) [ submitBtn ]
          
  in
    Html.form [ class "custom-form", onSubmit SubmitForm ] formContent
