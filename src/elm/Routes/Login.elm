module Routes.Login exposing
  ( Model
  , Msg
  , initModel
  , view
  , update
  )

import Html as H exposing (div, text, h1, Html)
import Html.Attributes as A exposing (class, value)
import Html.Events exposing (onInput, onSubmit)
import Http

import Api
import Api.Deserialize as Input
import SharedState exposing (SharedState, PublicState, SharedStateUpdate)
import UI.Hnav exposing (hnav)
import UI.Button as Btn exposing (button, link)
import Utils
  

type alias Model = 
  { username : String
  , password : String

  -- maybe rename to `locked`?
  , formSubmitting : Bool
  }


type Msg
  = UpdateUsername String
  | UpdatePassword String
  | SubmitForm
  | FormSubmitted (Result Http.Error Input.AdminWithToken)


initModel : Model
initModel = Model "" "" False




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

    FormSubmitted r ->
      let
        _ = Debug.log "FormSubmitted" r
      in
      (model, Cmd.none, SharedState.NoUpdate)



-- View

type alias Title = String


loginForm : Model -> Html Msg
loginForm model =
  let
    toClass = String.concat << List.intersperse " "

    classes = class <| toClass
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
        ] 

  in
  ( "Login", markup )
