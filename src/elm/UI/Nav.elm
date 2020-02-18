{--
Horizontal Nav
--}

module UI.Nav exposing
  ( withVnav
  , withHnav
  , init
  , update
  , NavState
  , Msg
  )

import Dict
import Browser.Navigation as Nav
import Html exposing (Html, div, header, nav, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import RemoteData

import SharedState exposing (PrivateState, SharedStateUpdate)
import UI.Button as Btn
import UI.Link as Link
import UI.Icons exposing (logo, hamburger, rightCaret, x)
import UI.Loader as Loader
import Utils as Utils


type alias NavState =
  { responsiveNavVisible : Bool
  , siteSummaryVisible : Bool
  }


type alias WithNavbar a =
  { a |
    navbar : NavState
  }

  
type Msg
  = LogOut PrivateState
  | ToggleResponsiveNavbar
  | ToggleShowSiteSummary


init : NavState
init =
  { responsiveNavVisible = True
  , siteSummaryVisible = False
  }


update : Msg -> WithNavbar a -> ( WithNavbar a, Cmd msg, SharedStateUpdate )
update msg ({ navbar } as parentModel) =
  case msg of
    ToggleResponsiveNavbar ->
      ( { parentModel |
          navbar =
            { navbar | responsiveNavVisible = not navbar.responsiveNavVisible
            }
        }
      , Cmd.none
      , SharedState.NoUpdate
      )

    ToggleShowSiteSummary ->
      ( { parentModel |
          navbar =
            { navbar | siteSummaryVisible = not navbar.siteSummaryVisible
            }
        }
      , Cmd.none
      , SharedState.NoUpdate
      )

    LogOut privateState ->
        let
          ( logOutCmd, sharedStateUpdate ) = Utils.logout privateState

        in
          ( parentModel
          , Cmd.batch [ logOutCmd, Nav.pushUrl privateState.navKey "/" ]
          , sharedStateUpdate
          )


hnav : List (Html msg) -> Html msg
hnav children =
  header [ class "w-auto h-20 pt-6 pb-2 px-4 flex justify-between" ]
    [ logo "45"
    , nav [] children
    ]



withVnav : PrivateState -> ({ a | navbar : NavState }) -> (Msg -> msg) -> Html msg -> Html msg
withVnav state { navbar } tagger pageContent =
  let
    loading =
      div [ class "loading-container" ]
        [ Loader.donut ]
        
    siteNav =
      case state.sites of
        RemoteData.Success sites ->
          let 
            bgColour = if SharedState.allVerified sites
              then
                "bg-gray-400"
              else
                "bg-red-300"

            classes = Utils.toClass
              [ "inline-block"
              , "bg-gray-400"
              , "py-1"
              , "px-2"
              , "text-center"
              , "rounded"
              , bgColour
              ]

            siteList =
              Dict.values sites
              |> List.map (\site ->
                Btn.link (Link.Site site.id) site.hostname
                |> Btn.secondary
                |> Btn.toHtml
              )
              
            siteSummary =
              let
                defaultClasses = [ "site-summary" ]
                
                siteSummaryClasses =
                  if navbar.siteSummaryVisible then
                    defaultClasses
                  else
                    "hidden" :: defaultClasses

              in
              div [ Utils.toClass siteSummaryClasses ]
                [ div [ class "p-1 border-b-2 border-solid border-gray-300" ]
                    [ text "Your Sites" ]
                , div [ class "my-4" ] siteList
                ]
          in
            div [ class "relative" ]
              [ div [ onClick (tagger ToggleShowSiteSummary), class "mx-5 my-2 p-2 text-center rounded cursor-pointer hover:bg-gray-300 select-none" ]
                  [ div [ ] [ text "Sites ", rightCaret ]
                  , div [ classes ] [ text <| String.fromInt (Dict.size sites) ]
                  ]
              , siteSummary
              ]
        
        _ -> loading

    navContent = 
      div [ class "font-bold flex flex-col items-center" ]
          [ div [] [ logo "40" ]
          , siteNav
          , Btn.button "Log Out"
            |> Btn.onClick (tagger <| LogOut state)
            |> Btn.toHtml
          ]

    regularNav =
      header
        [ class "hidden bg-gray-200 h-full w-48 p-5 md:block"
        ]
        [ navContent ]

    closeIcon =
      div [ onClick (tagger ToggleResponsiveNavbar) ]
        [ x ]

    responsiveNav = 
      if navbar.responsiveNavVisible then
        header
          [ class "bg-gray-100 fixed h-full w-2/3 p-5 md:hidden"
          ]
          [ closeIcon, navContent ]
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
