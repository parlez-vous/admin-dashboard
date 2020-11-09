module UI.Link exposing
    ( Route(..)
    , externalLink
    , link
    , toHref
    , toHtml
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Utils as Utils


type Route
    = Home
    | Dash
    | Site String
    | Login
    | Signup
    | RegisterSite


type alias Options =
    { route : Route
    }


type alias Label =
    String


type Link
    = Link Options Label


link : Route -> Label -> Link
link route =
    Link (Options route)


toHref : Route -> String
toHref route =
    case route of
        Home ->
            "/"

        Dash ->
            "/dash"

        Site id ->
            "/sites/" ++ id

        Login ->
            "/login"

        Signup ->
            "/signup"

        RegisterSite ->
            "/register-site"


linkStyles : List String
linkStyles =
    [ "text-blue-500"
    , "underline"
    , "hover:text-blue-700"
    ]


toHtml : Link -> Html msg
toHtml (Link opts label) =
    a [ href (toHref opts.route), Utils.toClass linkStyles ] [ text label ]


type alias URL =
    String


externalLink : URL -> String -> Html a
externalLink url label =
    a [ href url, Utils.toClass linkStyles ] [ text label ]
