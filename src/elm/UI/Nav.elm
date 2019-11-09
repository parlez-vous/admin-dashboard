{--
Horizontal Nav
--}

module UI.Nav exposing
  ( withVnav
  , withHnav
  , update
  , Msg
  )

import Html exposing (Html, div, header, nav)
import Html.Attributes as Attributes exposing (class)
import Html.Events exposing (onClick)
import UI.Icons exposing (logo, hamburger, x)


type alias Model a =
  { a |
    responsiveNavVisible : Bool
  }


type Msg
  = ToggleResponsiveNavbar


update : Msg -> Model a -> Model a
update _ model =
  { model |
    responsiveNavVisible = not model.responsiveNavVisible
  }


hnav : List (Html msg) -> Html msg
hnav children =
  header [ class "w-auto h-20 pt-6 pb-2 px-4 flex justify-between" ]
    [ logo "45"
    , nav [] children
    ]





withVnav : Model a -> (Msg -> msg) -> Html msg -> Html msg -> Html msg
withVnav { responsiveNavVisible } tagger navContent pageContent =
  let
    closeIcon =
      div [ onClick (tagger ToggleResponsiveNavbar) ]
        [ x ]

    navContents = [ navContent ]

    regularNav =
      header
        [ class "hidden bg-gray-200 h-full w-1/6 p-5 md:block"
        ]
        navContents

    responsiveNav = 
      if responsiveNavVisible then
        header
          [ class "bg-gray-100 fixed h-full w-2/3 p-5 md:hidden"
          ]
          (closeIcon :: navContents)
      else
        div
          [ class "m-5 flex justify-between md:hidden"
          , onClick (tagger ToggleResponsiveNavbar)
          ]
          [ hamburger ]

  in
    div [ class "h-full w-full md:flex" ]
      [ responsiveNav, regularNav, pageContent ]



withHnav : List (Html msg) -> List (Html msg) -> Html msg
withHnav items content =
  div [ class "h-full w-full"]
    [ hnav items
    , div [ class "w-full mx-4 md:mx-auto text-center" ]
        content
    ]
