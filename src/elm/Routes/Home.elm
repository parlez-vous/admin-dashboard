module Routes.Home exposing
  ( Model
  , FormType(..)
  , FormState(..)
  , Signin
  , Signup
  , Msg
  , update
  , view
  )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E

import Logo exposing (logo)
import Routes.Router as Router

-- MODEL

type alias Signup = 
  { username        : String
  , password        : String
  , passwordConfirm : String
  }

type alias Signin =
  { username : String
  , password : String
  }

type FormType
  = SignupForm Signup
  | SigninForm Signin

type FormState
  = ShowForm FormType
  | FormHidden
  | FormSubmitting

type alias Model =
  { key: Nav.Key
  , form: FormState
  }





-- MSG


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





-- UPDATE

updateFormField : FormType -> FormMsg -> FormType
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



handleSubmitForm : FormType -> Cmd Msg
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

updateForm : Model -> FormState -> Model
updateForm currentModel nextState =
  { currentModel | form = nextState }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    formUpdate = updateForm model
  
  in
    case msg of
      DisplayLogin ->
          let emptyForm = SigninForm <| Signin "" ""
          in
            (formUpdate <| ShowForm emptyForm, Cmd.none)

      DisplaySignup ->
        let emptyForm = SignupForm <| Signup "" "" ""
        in
          (formUpdate <| ShowForm emptyForm, Cmd.none)


      Form formMsg ->
        let
          updatedForm =
            case model.form of
              ShowForm form ->
                ShowForm <| updateFormField form formMsg
            
              -- this seems like a code smell
              -- this state should never occur
              _ -> model.form

        in
          (formUpdate updatedForm, Cmd.none)

      SubmitForm ->
        let httpCmd =
              case model.form of
                ShowForm form ->
                  handleSubmitForm form
                
                -- this seems like a code smell
                -- this state should never occur
                _ -> Cmd.none

        in
          ( formUpdate FormSubmitting, httpCmd )


      SubmittedForm result ->
        case result of
          Ok _ ->
            (Debug.log "success!" model, Nav.pushUrl model.key "/admin")

          Err _ ->
            (Debug.log "failure!" model, Cmd.none)







-- View


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
      case model.form of
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
      case model.form of
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


{-

view : Model -> Html msg
view model =
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
-}

view : Model -> Router.Details Msg
view model =
  { title = "Parlez-Vous | Home"
  , header = cta
  , children = [
      div [ class "container" ]
        [ div [ class "row" ]
            [ h1 [ class "center-text slogan" ] [ text "Enable Conversations"]
            , pre [ class "center-text" ] [ text "work in progress" ]
            , div [ class "logo-container" ] [ logo ]
            , p [ class "center-text" ] [ text "The fastest way to engage your audience" ]
            , form_ model
            ]
        ]
    ]
  }
