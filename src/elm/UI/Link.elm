module UI.Link exposing
  ( link
  , toHtml
  , toHref
  , Route(..)
  )


import Html exposing (..)
import Html.Attributes exposing (..)

type Route
  = Home
  | Dash
  | Site Int
  | Login
  | Signup
  | RegisterSite


type alias Options =
  { route : Route
  }

type alias Label = String

type Link
  = Link Options Label


link : Route -> Label -> Link
link route = Link (Options route)


toHref : Route -> String
toHref route = case route of
  Home -> "/"
  Dash -> "/dash"
  Site id -> "/sites/" ++ String.fromInt id
  Login -> "/login"
  Signup -> "/signup"
  RegisterSite -> "/register-site"


toHtml : Link -> Html msg
toHtml (Link opts label) =
  a [ href (toHref opts.route) ] [ text label ]
