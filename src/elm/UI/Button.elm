module UI.Button exposing
  ( button
  , secondary
  , disabled
  , toHtml
  )

import Html exposing (Html, text)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events exposing (onClick)


type alias Options =
  { disabled : Bool
  , loading : Bool
  , primary : Bool
  -- icon : Icon
  -- size : Size (Small, Medium, Large)
  -- etc etc
  }


defaultOptions : Options
defaultOptions =
  { disabled = False
  , loading = False
  , primary = True
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
  

secondary : Button msg -> Button msg
secondary (Button opts msg label) = 
  let
    newOpts = { opts | primary = False }
  in
    Button newOpts msg label


toHtml : Button msg -> Html msg
toHtml (Button opts msg label) =
  let
    toClass : List String -> String
    toClass = String.concat << List.intersperse " "

    primaryStyles =
      [ "text-gray-600"
      , "bg-gray-300"
      , "px-4"
      , "py-2"
      , "hover:bg-gray-400"
      , "hover:text-gray-700"
      , "rounded"
      ]

    conditionalClasses = classList
      [ ("disabled", opts.disabled)
      , ("loading", opts.loading)
      ]

  in
    Html.button
      [ class <| toClass primaryStyles
      , conditionalClasses
      , onClick msg
      , Attributes.disabled opts.disabled
      ]
      [ text label ]
