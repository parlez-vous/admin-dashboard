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
import Routes.Login as Login
import Routes.Signup as Signup
import SharedState exposing (SharedState(..), SharedStateUpdate)



type Route
  = Home Home.Model
  | Dash Dash.Model
  | Site Site.Model
  | Login Login.Model
  | Signup Signup.Model
  | NotFound


type alias Model = Route


type Msg
  = UrlChange Url
  | HomeMsg Home.Msg
  | DashMsg Dash.Msg
  | SiteMsg Site.Msg
  | LoginMsg Login.Msg
  | SignupMsg Signup.Msg


parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map (Home Home.initModel) Parser.top
    , Parser.map (Dash Dash.initModel) (Parser.s "dash")
    , Parser.map (Site << Site.initModel) (Parser.s "sites" </> int)
    , Parser.map (Login Login.initModel) (Parser.s "login")
    , Parser.map (Signup Signup.initModel) (Parser.s "signup")
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
      |> Cmd.map DashMsg

    ( Site siteModel, Private privateState ) ->
      Site.transitionTrigger siteModel privateState
      |> Cmd.map SiteMsg

    -- redirect guests on private routes
    ( Dash _, Public { navKey } ) ->
      Nav.pushUrl navKey "/"

    ( Site _, Public { navKey } ) ->
      Nav.pushUrl navKey "/"

    ( Login _, Private { navKey } ) ->
      Nav.pushUrl navKey "/dash"

    ( Signup _, Private { navKey } ) ->
      Nav.pushUrl navKey "/dash"
    
    _ -> Cmd.none
      


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update state msg model =
  case (msg, model) of
    ( UrlChange url, _ ) -> 
      let
        route = fromUrl url

        transitionTriggerMsg = transitionTrigger route state

      in
      ( route
      , transitionTriggerMsg
      , SharedState.NoUpdate
      )
      
    ( HomeMsg homeMsg, Home homeModel ) ->
      let 
        ( newModel, homeCmd, sharedStateUpdate ) =
          Home.update state homeMsg homeModel
      in
        ( Home newModel
        , Cmd.map HomeMsg homeCmd
        , sharedStateUpdate
        )

    ( DashMsg dashMsg, Dash dashModel ) ->
      case state of
        Private privateState ->
          let
            ( newModel, dashCmd, sharedStateUpdate ) =
              Dash.update privateState dashMsg dashModel
          in
          ( Dash newModel
          , Cmd.map DashMsg dashCmd
          , sharedStateUpdate
          )

        Public publicState ->
          ( Dash dashModel
          , Cmd.none
          , SharedState.NoUpdate
          )
    
    ( SiteMsg siteMsg, Site siteModel ) ->
      case state of
        Private privateState ->
          let
            ( newModel, siteCmd, sharedStateUpdate ) =
              Site.update privateState siteMsg siteModel
          in
            ( Site newModel
            , Cmd.map SiteMsg siteCmd
            , sharedStateUpdate
            )

        Public _ ->
          ( Site siteModel
          , Cmd.none
          , SharedState.NoUpdate
          )

    ( LoginMsg loginMsg, Login loginModel ) ->
      let
        ( newModel, loginCmd, sharedStateUpdate ) = 
          Login.update state loginMsg loginModel
      
      in
        ( Login newModel
        , Cmd.map LoginMsg loginCmd
        , sharedStateUpdate
        )


    -- Placeholder for now
    _ ->
      ( model, Cmd.none, SharedState.NoUpdate )

    
{-- Generalize the following
  Site.view privateState siteModel
    |> Tuple.mapSecond (Html.map <| SiteMsg siteModel)
    |> Tuple.mapSecond (Html.map toMsg)
--}


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
          |> Tuple.mapSecond (Html.map HomeMsg)
          |> Tuple.mapSecond (Html.map toMsg)
          

        Dash dashModel ->
          case sharedState of
            Public _ -> redirectPage
            
            Private privateState -> 
              Dash.view privateState dashModel
              |> Tuple.mapSecond (Html.map DashMsg)
              |> Tuple.mapSecond (Html.map toMsg)


        Site siteModel ->
          case sharedState of
            Public _ -> redirectPage

            Private privateState ->
              Site.view privateState siteModel
              |> Tuple.mapSecond (Html.map SiteMsg)
              |> Tuple.mapSecond (Html.map toMsg)

        NotFound ->
          ( "Woops!", div [] [ text "404 Not Found"] )

        Login loginModel ->
          case sharedState of
            Private _ -> redirectPage

            Public publicState ->
              Login.view publicState loginModel
              |> Tuple.mapSecond (Html.map LoginMsg)
              |> Tuple.mapSecond (Html.map toMsg)
          

        _ ->
          ( "Placeholder", div [] [ text "Hello :)" ])

  in
  { title = title ++ " | Parlez-Vous "
  , body = [ html ]
  }