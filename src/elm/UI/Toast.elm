module UI.Toast exposing
  ( ToastState
  , ToastMsg
  , init
  , update
  , addToast
  , view
  )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Toasty exposing (Stack)

import Utils


type alias ToastState = Stack String
type alias ToastMsg = Toasty.Msg String

type alias WithToasts a =
  { a |
    toasts : ToastState
  }

init : ToastState
init = Toasty.initialState


config : Toasty.Config msg
config = Toasty.config





addToast : (Toasty.Msg String -> a) -> String -> ( WithToasts b, Cmd a) -> ( WithToasts b, Cmd a)
addToast tagger toast (model, cmd) =
  let
    toasties_ =
      { toasties = model.toasts }

    ({ toasties }, newCmd ) = Toasty.addToast config tagger toast (toasties_, cmd)
  in
    ( { model | toasts = toasties }
    , newCmd
    )




update : (Toasty.Msg String -> a) -> Toasty.Msg String -> WithToasts b -> (WithToasts b, Cmd a)
update tagger toastInfo model =
  let
    toasties_ =
      { toasties = model.toasts
      }

    ({ toasties }, cmd ) = Toasty.update config tagger toastInfo toasties_
  in
    ( { model | toasts = toasties }
    , cmd
    )


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
