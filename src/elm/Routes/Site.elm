module Routes.Site exposing
    ( Model
    , Msg(..)
    , initModel
    , update
    , view
    )

import Ant.Typography as T
import Api.Deserialize exposing (Comment, Comments, Site)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import RemoteData exposing (WebData)
import SharedState exposing (PrivateState, SharedStateUpdate(..))
import UI.Card exposing (card)
import UI.Nav as ResponsiveNav exposing (withVnav)


type alias Model =
    { siteId : String
    , navbar : ResponsiveNav.NavState
    , comments : WebData Comments
    }


type Msg
    = ResponsiveNavMsg ResponsiveNav.Msg
    | LoadComments (WebData Comments)


initModel : String -> Model
initModel siteId =
    Model siteId ResponsiveNav.init RemoteData.NotAsked


update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update _ msg model =
    case msg of
        ResponsiveNavMsg subMsg ->
            ResponsiveNav.update subMsg model

        LoadComments data ->
            ( { model | comments = data }, Cmd.none, NoUpdate )


type alias Title =
    String


view : PrivateState -> Model -> ( Title, Html Msg )
view state model =
    let
        viewWithNav =
            withVnav state model ResponsiveNavMsg

        maybeSite =
            case state.sites of
                RemoteData.Success sites_ ->
                    Dict.get model.siteId sites_

                _ ->
                    Nothing

        info =
            case maybeSite of
                Nothing ->
                    text "Site not found"

                Just site ->
                    siteView site model.comments
    in
    ( "Site"
    , viewWithNav info
    )


siteView : Site -> WebData Comments -> Html msg
siteView site comments =
    div [ class "m-2 md:m-12 w-full" ]
        [ site.hostname |> T.title |> T.toHtml
        , "Comments" |> T.title |> T.level T.H2 |> T.toHtml
        , commentsView comments
        ]


commentsView : RemoteData.WebData Comments -> Html msg
commentsView commentsData =
    div [] <|
        case commentsData of
            RemoteData.NotAsked ->
                [ div []
                    [ text "Loading..." ]
                ]

            RemoteData.Loading ->
                [ div []
                    [ text "Loading..." ]
                ]

            RemoteData.Success comments ->
                List.map commentView comments

            _ ->
                [ div [] [ text "Some error" ] ]


commentView : Comment -> Html msg
commentView comment =
    card
        [ text comment.body
        ]
