module Main exposing (main)

import Browser
import Html exposing (
  Html,
  button,
  div,
  text,
  pre,
  form,
  label,
  input,
  img,
  h1,
  p)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (
  id,
  class,
  value,
  disabled,
  for,
  src,
  placeholder,
  type_)
  
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
  | Hidden


type alias Model = Form


-- type alias Model = Int

init : () -> (Model, Cmd msg)
init _ = (Hidden, Cmd.none)


-- UPDATE


updateFormField : Form -> FormMsg -> Form
updateFormField currentForm msg =
  case currentForm of
    
    Hidden -> currentForm

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



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DisplayLogin ->
      let emptyForm = SigninForm <| Signin "" ""
      in
        (emptyForm, Cmd.none)

    DisplaySignup ->
      let emptyForm = SignupForm <| Signup "" "" "" ""
      in
        (emptyForm, Cmd.none)


    Form formMsg ->
      let
        updatedModel = updateFormField model formMsg

      in
        (updatedModel, Cmd.none)

    SubmitForm ->
      (Debug.log "submitting" model, Cmd.none)





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
  div [ class "container" ]
    [ div [ class "row" ]
        [ h1 [ class "center-text slogan" ] [ text "Enable Conversations"]
        , pre [ class "center-text" ] [ text "work in progress" ]
        , div [ class "logo-container" ] [ logo ]
        , p [ class "center-text" ] [ text "The fastest way to engage your audience" ]
        , cta
        , form_ model
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
        Hidden -> False

        SignupForm data -> 
          String.length data.email > 3 &&
          String.length data.website > 5 &&
          String.length data.password > 6 &&
          data.password == data.passwordConfirm

        SigninForm data ->
          String.length data.email > 3 &&
          String.length data.password > 6

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
        Hidden -> []

        SignupForm data ->
          List.append (baseForm data)
            [ label [ for "password-confirm-input" ] [ text "confirm password" ]
            , input [ id "password-confirm-input", type_ "password", onInput (Form << PasswordConfirm) ] [] 

            , label [ for "website-input" ] [ text "your website" ]
            , input [ id "website-input", type_ "text", onInput (Form << WebsiteInput) ] [] 

            , submitBtn
            ]

        SigninForm data ->
          List.append (baseForm data) [ submitBtn ]
          
  in
    form [ class "custom-form", onSubmit SubmitForm ] formContent
