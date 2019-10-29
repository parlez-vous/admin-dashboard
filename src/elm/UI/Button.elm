module UI.Button exposing
  ( button
  , link
  , onClick
  , Route(..)
  , secondary
  , disabled
  , toHtml
  )

import Html exposing (Html, text)
import Html.Attributes as Attributes exposing (class, classList, href)
import Html.Events as E




type Route
  = Home
  | Dash
  | Site Int
  | Login
  | Signup


type alias Options msg =
  { disabled : Bool
  , loading : Bool
  , primary : Bool
  , href : Maybe Route
  , onClick : Maybe msg
  -- icon : Icon
  -- size : Size (Small, Medium, Large)
  -- etc etc
  }


defaultOptions : Options msg
defaultOptions =
  { disabled = False
  , loading = False
  , primary = True
  , href = Nothing
  , onClick = Nothing
  }


type Button msg
  = Button (Options msg) String


onClick : msg -> Button msg -> Button msg
onClick msg (Button opts label) =
  let
    newOpts = { opts | onClick = Just msg }
  in
    Button newOpts label


withHref : Route -> Button msg -> Button msg
withHref route (Button opts label) =
  let
    newOpts = { opts | href = Just route }
  in
    Button newOpts label


button : String -> Button msg
button label =
  Button defaultOptions label


link : Route -> String -> Button msg
link route label = 
  Button defaultOptions label
  |> withHref route



disabled : Bool -> Button msg -> Button msg
disabled state (Button opts label) =
  let
    newOpts = { opts | disabled = state }
  in
    Button newOpts label
  

secondary : Button msg -> Button msg
secondary (Button opts label) = 
  let
    newOpts = { opts | primary = False }
  in
    Button newOpts label


toHref : Route -> String
toHref route = case route of
  Home -> "/"
  Dash -> "/dash"
  Site id -> "/sites/" ++ String.fromInt id
  Login -> "/login"
  Signup -> "/signup"


toHtml : Button msg -> Html msg
toHtml (Button opts label) =
  let
    toClass : List String -> String
    toClass = String.concat << List.intersperse " "

    sharedStyles = 
      [ "font-bold"
      , "py-2"
      , "px-4"
      , "rounded"
      , "mx-2"
      ]

    secondaryStyles =
      [ "text-gray-600"
      , "hover:bg-gray-300"
      ]

    primaryStyles =
      [ "text-white"
      , "bg-purple-500"
      , "hover:bg-purple-400"
      ]

    buttonStyles = List.append sharedStyles (
      if opts.primary then
        primaryStyles
      else
        secondaryStyles)

    conditionalClasses = classList
      [ ("disabled", opts.disabled)
      , ("loading", opts.loading)
      ]

    defaultAttrs = 
      [ class <| toClass buttonStyles
      , conditionalClasses
      , Attributes.disabled opts.disabled
      ]

    elem = case opts.href of
      Nothing ->
        let
          btnAttrs = 
            case opts.onClick of
              Nothing -> defaultAttrs
              Just msg -> (E.onClick msg) :: defaultAttrs
        in
        Html.button btnAttrs

      Just route ->
        let
          linkAttrs = (href <| toHref route) :: defaultAttrs
        in
        Html.a linkAttrs

  in
    elem [ text label ]
