module Router exposing
  ( Model
  , Msg(..)
  , init
  , transitionTrigger
  , update
  , view
  )


import Html as Html exposing (..)
import Browser
import Browser.Navigation as Nav
import RemoteData
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, int, (</>))



import Routes.Home as Home
import Routes.Dash as Dash
import Routes.Site as Site
import SharedState exposing (SharedState(..), SharedStateUpdate)



{-
Once the application gets more complex,
you can import the models from various routes

import Routes.Home as Home
import Routes.Settings as Settings

type alias Model =
  { home     : Home.Model
  , settings : Settings.Model
  , .....
  , route    : Route
  }
-}


type Route
  = Home Home.Model
  | Dash Dash.Model
  | Site Site.Model
  | NotFound


type alias Model = Route


type Msg
  = UrlChange Url
  | HomeMsg Home.Model Home.Msg
  | DashMsg Dash.Model Dash.Msg
  | SiteMsg Site.Model Site.Msg


parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map (Home Home.initModel) Parser.top
    , Parser.map (Dash Dash.initModel) (Parser.s "dash")
    , Parser.map (Site << Site.initModel) (Parser.s "sites" </> int)
    ]

fromUrl : Url -> Model
fromUrl = Maybe.withDefault NotFound << Parser.parse parser


init : Url -> SharedState -> ( Model, Cmd Msg )
init url sharedState =
  let
    model = fromUrl url

  in
  ( model
  , transitionTrigger model sharedState
  )



-- trigger commands on page transitions
-- and initializations
transitionTrigger : Route -> SharedState -> Cmd Msg
transitionTrigger route state =
  case ( route, state ) of
    ( Dash dashModel , Private privateState ) ->
      Dash.transitionTrigger privateState
      |> Cmd.map (DashMsg dashModel)

    ( Site siteModel, Private privateState ) ->
      Site.transitionTrigger siteModel privateState
      |> Cmd.map (SiteMsg siteModel)

    -- redirect guests on private routes
    ( Dash _, Public { navKey } ) ->
      Nav.pushUrl navKey "/"

    ( Site _, Public { navKey } ) ->
      Nav.pushUrl navKey "/"
    
    _ -> Cmd.none
      


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case msg of
    UrlChange url -> 
      let
        route = fromUrl url

        transitionTriggerMsg = transitionTrigger route state

      in
      ( route
      , transitionTriggerMsg
      , SharedState.NoUpdate
      )
      
    HomeMsg homeModel homeMsg ->
      let 
        ( newModel, homeCmd, sharedStateUpdate ) =
          Home.update state homeMsg homeModel
      in
        ( Home newModel
        , Cmd.map (HomeMsg newModel) homeCmd
        , sharedStateUpdate
        )

    DashMsg dashModel dashMsg ->
      case state of
        Private privateState ->
          let
            ( newModel, dashCmd, sharedStateUpdate ) =
              Dash.update privateState dashMsg dashModel
          in
          ( Dash newModel
          , Cmd.map (DashMsg newModel) dashCmd
          , sharedStateUpdate
          )

        Public publicState ->
          ( Dash dashModel
          , Cmd.none
          , SharedState.NoUpdate
          )
    
    SiteMsg siteModel siteMsg ->
      case state of
        Private privateState ->
          let
            ( newModel, siteCmd, sharedStateUpdate ) =
              Site.update privateState siteMsg siteModel
          in
            ( Site newModel
            , Cmd.map (SiteMsg newModel) siteCmd
            , sharedStateUpdate
            )

        Public _ ->
          ( Site siteModel
          , Cmd.none
          , SharedState.NoUpdate
          )



view : (Msg -> msg) -> SharedState -> Model -> Browser.Document msg
view toMsg sharedState routerModel =
  let
    redirectPage =
      ( "Redirecting ..."
      , div [] [ text "Redirecting ..."]
      )

    ( title, html ) =
      case routerModel of
        Home homeModel ->
          Home.view sharedState homeModel
          |> Tuple.mapSecond (Html.map <| HomeMsg homeModel)
          |> Tuple.mapSecond (Html.map toMsg)
          

        Dash dashModel ->
          case sharedState of
            Public _ -> redirectPage
            
            Private privateState -> 
              Dash.view privateState dashModel
              |> Tuple.mapSecond (Html.map <| DashMsg dashModel)
              |> Tuple.mapSecond (Html.map toMsg)


        Site siteModel ->
          case sharedState of
            Public _ -> redirectPage

            Private privateState ->
              Site.view privateState siteModel
              |> Tuple.mapSecond (Html.map <| SiteMsg siteModel)
              |> Tuple.mapSecond (Html.map toMsg)
            
        NotFound ->
          ( "Woops!", div [] [ text "404 Not Found"] )

  in
  { title = title ++ " | Parlez-Vous "
  , body = [ html ]
  }