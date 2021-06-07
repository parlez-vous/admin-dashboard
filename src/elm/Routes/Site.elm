module Routes.Site exposing
    ( Model
    , Msg(..)
    , initModel
    , update
    , view
    )

import Ant.Typography as T
import Api.Deserialize exposing (Author(..), Comment, Comments, Site)
import Date
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import RemoteData exposing (WebData)
import SharedState exposing (PrivateState, SharedStateUpdate(..))
import Time
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


view : ( { a | timezone : Time.Zone }, PrivateState ) -> Model -> ( Title, Html Msg )
view ( generalData, state ) model =
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
                    siteView generalData.timezone site model.comments
    in
    ( "Site"
    , viewWithNav info
    )


siteView : Time.Zone -> Site -> WebData Comments -> Html msg
siteView zone site comments =
    div [ class "m-2 md:m-12 w-full" ]
        [ site.hostname |> T.title |> T.toHtml
        , "Comments" |> T.title |> T.level T.H2 |> T.toHtml
        , commentsView zone comments
        ]


commentsView : Time.Zone -> RemoteData.WebData Comments -> Html msg
commentsView zone commentsData =
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
                List.map (commentView zone) comments
                    |> ifEmpty
                        [ card
                            "bg-indigo-200"
                            [ text "No comments to show" ]
                        ]

            _ ->
                [ div [] [ text "Some error" ] ]


ifEmpty : List a -> List a -> List a
ifEmpty arg1 arg2 =
    if List.isEmpty arg2 then
        arg1

    else
        arg2


timeFormat : Time.Zone -> Time.Posix -> String
timeFormat zone time =
    time |> Date.fromPosix zone |> Date.format "MMM dd, yyyy"


authorName : Author -> String
authorName author =
    case author of
        Anonymous name ->
            name

        Known name ->
            name


commentView : Time.Zone -> Comment -> Html msg
commentView zone comment =
    card ""
        [ div [ class "mb-4 text-gray-600" ] [ comment.createdAt |> timeFormat zone |> text ]
        , div []
            [ span [ class "text-blue-500" ] [ "@" ++ (comment.author |> authorName) |> text ]
            , span [ class "italic" ] [ text " commented on " ]
            , span [ class "text-blue-500" ] [ comment.post.id |> text ]
            ]
        , div [ class "mt-4" ] [ text comment.body ]
        ]
