module UI.Toast exposing
  ( ToastState
  , init
  , config
  , view
  )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Toasty exposing (Stack)

import Utils


type alias ToastState = Stack String


init : ToastState
init = Toasty.initialState


config : Toasty.Config msg
config = Toasty.config


-- TODO: customize inset to provide some space
-- between the bottom and the toast on wider
view : (Toasty.Msg String -> msg) -> ToastState -> Html msg
view toMsg toastStack =
  div [ class "fixed inset-x-0 bottom-0" ]
    [ Toasty.view config renderToast toMsg toastStack ]
    

renderToast : String -> Html msg
renderToast toast =
  let
    classes = Utils.toClass
      [ "py-1"
      , "px-2"
      , "bg-red-200"
      , "text-red-700"
      , "rounded"
      , "mb-4"
      , "md:mx-auto"
      , "md:w-1/6"
      ]
  in
  div [ classes ] [ text toast ]
