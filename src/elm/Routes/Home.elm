port module Routes.Home exposing
  ( Model
  , FormType(..)
  , FormState(..)
  , Msg
  , init
  , update
  , view
  )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (..)
import Http


import Api
import Api.Output as Output
import Api.Deserialize as Input
import Logo exposing (logo)
import Session
import SharedState exposing (SharedState, SharedStateUpdate)



-- MODEL


type FormType
  = SignupForm Output.Signup
  | SigninForm Output.Signin

type FormState
  = ShowForm FormType
  | FormHidden
  | FormSubmitting

type alias Model =
  { form: FormState
  }




-- PORTS

port setToken : Input.SessionToken -> Cmd msg



-- MSG


type FormMsg
  = UsernameInput String
  | PasswordInput String
  | PasswordConfirm String


type Msg
  = DisplayLogin
  | DisplaySignup
  | Form FormMsg
  | SubmitForm FormType
  | SubmittedForm (Result Http.Error Input.AdminWithToken)


init : Model
init = Model FormHidden

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



handleSubmitForm : String -> FormType -> Cmd Msg
handleSubmitForm api form =
  case form of 
    SignupForm data ->
      Api.adminSignup api SubmittedForm data

    SigninForm data ->
      Api.adminSignin api SubmittedForm data


updateForm : Model -> FormState -> Model
updateForm currentModel nextState =
  { currentModel | form = nextState }

update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
  let
    formUpdate = updateForm model
  
  in
    case msg of
      DisplayLogin ->
          let emptyForm = SigninForm <| Output.Signin "" ""
          in
            ( formUpdate <| ShowForm emptyForm
            , Cmd.none
            , SharedState.NoUpdate
            )

      DisplaySignup ->
        let emptyForm = SignupForm <| Output.Signup "" "" ""
        in
          ( formUpdate <| ShowForm emptyForm
          , Cmd.none
          , SharedState.NoUpdate
          )


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
          ( formUpdate updatedForm
          , Cmd.none
          , SharedState.NoUpdate
          )

      SubmitForm form ->
        ( formUpdate FormSubmitting
        , handleSubmitForm sharedState.api form
        , SharedState.NoUpdate
        )


      SubmittedForm result ->
        case result of
          Ok adminWithToken ->
            let
              commands =
                Cmd.batch
                  [ setToken <| Tuple.second adminWithToken
                  , Nav.pushUrl sharedState.navKey "/admin"
                  ]
            in
            ( Debug.log "success!" model
            , commands
            , SharedState.UpdateSession <| Session.Admin adminWithToken
            )

          Err _ ->
            ( Debug.log "failure!" model,
              Cmd.none,
              SharedState.NoUpdate
            )







-- View


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
    let
      action =
        case model.form of
          ShowForm formData -> [ onSubmit (SubmitForm formData) ]
          _                 -> []

    in
    Html.form
      ([ class "custom-form" ] ++ action)
      formContent



type alias Title = String

view : Model -> (Title, Html Msg)
view model =
  let
    cta =
      div [ class "cta" ]
        [ button [ onClick DisplayLogin ] [ text "log in" ]
        , button [ class "button-primary", onClick DisplaySignup ]
            [ text "sign up"]
        ]

    html =
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

  in
    ( "Home", html )

