module Routes.Site exposing
    ( Model
    , Msg
    , initModel
    , update
    , view
    )

import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import RemoteData
import SharedState exposing (PrivateState, SharedStateUpdate)
import UI.Nav as ResponsiveNav exposing (withVnav)


type alias Model =
    { siteId : String
    , navbar : ResponsiveNav.NavState
    }


type Msg
    = ResponsiveNavMsg ResponsiveNav.Msg


initModel : String -> Model
initModel siteId =
    Model siteId ResponsiveNav.init


update : PrivateState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update _ msg model =
    case msg of
        ResponsiveNavMsg subMsg ->
            ResponsiveNav.update subMsg model


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
                    text "site not found"

                Just site ->
                    div [ class "m-2 md:m-12 w-full" ]
                        [ h1 [ class "text-2xl" ] [ text site.hostname ]
                        , div [] [ text "ayyy you all good!" ]
                        ]
    in
    ( "Site"
    , viewWithNav info
    )
