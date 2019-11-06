{--
Horizontal Nav
--}

module UI.Nav exposing
  ( hnav
  , vnav
  , withVnav
  , withHnav
  )

import Html exposing (Html, div, header, nav)
import Html.Attributes as Attributes exposing (class)
import UI.Icons exposing (logo)

hnav : List (Html msg) -> Html msg
hnav children =
  header [ class "w-auto h-20 pt-6 pb-2 px-4 flex justify-between" ]
    [ logo "45"
    , nav [] children
    ]


vnav : Html msg -> Html msg -> Html msg
vnav primaryItems secondaryItems =
  header [ ]
    [ logo "45"
    , nav [] [ primaryItems ]
    , nav [] [ secondaryItems ]
    ]

withVnav : Html msg -> Html msg -> Html msg -> Html msg
withVnav primaryItems secondaryItems content =
  div [ class "h-full" ]
    [ vnav primaryItems secondaryItems
    , content
    ]


withHnav : List (Html msg) -> List (Html msg) -> Html msg
withHnav items content =
  div [ class "h-full w-full"]
    [ hnav items
    , div [ class "w-full mx-4 md:mx-auto text-center" ]
        content
    ]
