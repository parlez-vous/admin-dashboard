module UI.Button exposing
  ( button
  , disabled
  , toHtml
  )

import Html exposing (Html, text)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events exposing (onClick)


type alias Options =
  { disabled : Bool
  , loading : Bool
  -- icon : Icon
  -- size : Size (Small, Medium, Large)
  -- etc etc
  }


defaultOptions : Options
defaultOptions =
  { disabled = False
  , loading = False
  }


type Button msg
  = Button Options msg String


button : msg -> String -> Button msg
button msg label =
  Button defaultOptions msg label


disabled : Bool -> Button msg -> Button msg
disabled state (Button opts msg label) =
  let
    newOpts = { opts | disabled = state }
  in
    Button newOpts msg label
  

toHtml : Button msg -> Html msg
toHtml (Button opts msg label) =
  let
    conditionalClasses = classList
      [ ("disabled", opts.disabled)
      , ("loading", opts.loading)
      ]

  in
    Html.button
      [ class "btn__primary"
      , conditionalClasses
      , onClick msg
      , Attributes.disabled opts.disabled
      ]
      [ text label ]
