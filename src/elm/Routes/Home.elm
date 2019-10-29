port module Routes.Home exposing
  ( Model
  , Msg
  , initModel
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
import UI.Icons exposing (logo)
import Utils exposing (logout, getApi)
import SharedState exposing (SharedState(..), SharedStateUpdate)
import UI.Loader as Loader
import UI.Button as Btn exposing (link)
import UI.Hnav exposing (hnav)



-- MODEL


type FormType
  = SignupForm Output.Signup

type FormState
  = ShowForm FormType
  | FormHidden
  | FormSubmitting


type alias Model = FormState

-- PORTS

port setToken : String -> Cmd msg



-- MSG


type FormMsg
  = UsernameInput String
  | PasswordInput String
  | PasswordConfirm String


type Msg
  = DisplaySignup
  | Form FormMsg
  | SubmitForm FormType
  | SubmittedForm (Result Http.Error Input.AdminWithToken)
  | GoToDashboard
  | LogOut


initModel : Model
initModel = FormHidden

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



getNavKey : SharedState -> Nav.Key
getNavKey sharedState =
  case sharedState of
    Public { navKey } -> navKey
    Private { navKey } -> navKey


handleSubmitForm : String -> FormType -> Cmd Msg
handleSubmitForm api form =
  case form of 
    SignupForm data ->
      Api.adminSignup api SubmittedForm data


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
  case msg of
    DisplaySignup ->
      let emptyForm = SignupForm <| Output.Signup "" "" ""
      in
        ( ShowForm emptyForm
        , Cmd.none
        , SharedState.NoUpdate
        )


    Form formMsg ->
      let
        updatedForm =
          case model of
            ShowForm form ->
              ShowForm <| updateFormField form formMsg
          
            -- this seems like a code smell
            -- this state should never occur
            _ -> model

      in
        ( updatedForm
        , Cmd.none
        , SharedState.NoUpdate
        )

    SubmitForm form ->
      let
        api = getApi sharedState
      
      in
      ( FormSubmitting
      , handleSubmitForm api form
      , SharedState.NoUpdate
      )


    SubmittedForm result ->
      case result of
        Ok adminWithToken ->
          let
            navKey = getNavKey sharedState

            commands =
              Cmd.batch
                [ setToken <| Tuple.second adminWithToken
                , Nav.pushUrl navKey "/dash"
                ]
          in
          ( FormHidden
          , commands
          , SharedState.SetAdmin adminWithToken
          )

        Err _ ->
          ( Debug.log "failure!" model,
            Cmd.none,
            SharedState.NoUpdate
          )

    GoToDashboard ->
      let
        navKey = getNavKey sharedState
      in
      ( model
      , Nav.pushUrl navKey "/dash"
      , SharedState.NoUpdate
      )

    LogOut ->
      let
        ( cmd, sharedStateUpdate ) =
          case sharedState of
            -- logging out from an unauthenticated state
            -- does not make sense
            Public _ -> ( Cmd.none, SharedState.NoUpdate )

            Private { api, navKey } ->
              logout { api = api, navKey = navKey }

      in
      ( model
      , cmd
      , sharedStateUpdate
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
      case model of
        ShowForm form ->
          case form of
            SignupForm data -> 
              String.length data.username > 3 &&
              String.length data.password > 6 &&
              data.password == data.passwordConfirm

        _ -> False

    submitBtn =
      if readyToSubmit
      then
        button [ ] [ text "submit" ]
      else
        div [] []

    action =
      case model of
        ShowForm formData -> [ onSubmit (SubmitForm formData) ]
        _                 -> []
      

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
          
  in
    Html.form
      ([ class "custom-form" ] ++ action)
      formContent



type alias Title = String

view : SharedState -> Model -> (Title, Html Msg)
view sharedState model =
  let
    ctaButtons =
      case sharedState of
        Private _ -> 
          [ button [ onClick LogOut ] [ text "log out" ]
          , Btn.button "Go To Dashboard"
            |> Btn.onClick GoToDashboard
            |> Btn.toHtml
          ]

        Public _ ->
          [ Btn.toHtml <| link Btn.Login "log in"
          , Btn.toHtml <| link Btn.Signup "sign up"
          ]


    html =
      div [ class "home-page" ]
        [ hnav ctaButtons
        , div [ class "container" ]
            [ div [ class "row" ]
                [ h1 [ class "center-text slogan" ] [ text "Enable Conversations"]
                , pre [ class "center-text" ] [ text "work in progress" ]
                , div [ class "logo-container" ] [ logo "125" ]
                , p [ class "center-text" ] [ text "The fastest way to engage your audience" ]
                , form_ model
                ]
            ]
        ]

  in
    ( "Home", html )

