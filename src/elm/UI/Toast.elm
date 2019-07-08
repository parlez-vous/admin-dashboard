module UI.Toast exposing
  ( ToastState
  , init
  , config
  , view
  )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Toasty exposing (Stack)


type alias ToastState = Stack String


init : ToastState
init = Toasty.initialState


config : Toasty.Config msg
config = Toasty.config


view : (Toasty.Msg String -> msg) -> ToastState -> Html msg
view toMsg toastStack =
  div [ class "toast__container" ]
    [ Toasty.view config renderToast toMsg toastStack ]
    

renderToast : String -> Html msg
renderToast toast =
  div [ class "toast__item" ] [ text toast ]
