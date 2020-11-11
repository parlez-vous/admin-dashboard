module Api exposing
    ( Api
    , ApiClient
    , apiFactory
    , getApiClient
    )

import Api.Deserialize as Input
import Api.Output as Output
import Http
import Json.Decode as D
import Json.Encode as E
import RemoteData
import Url exposing (Url)
import Url.Builder


type Api
    = Api Url


type alias ApiClient msg =
    { adminSignUp : AdminSignUp msg
    , adminLogIn : AdminLogIn msg
    , getAdminSession : GetAdminSession msg
    , getManySites : GetManySites msg
    , getSite : GetSite msg
    , registerSite : RegisterSite msg
    }


type alias ToMsg a msg =
    Result Http.Error a -> msg


type alias RequestTemplate =
    { method : String
    , headers : List Http.Header
    , tracker : Maybe String
    , timeout : Maybe Float
    }


type Method
    = Get
    | Post


apiFactory : Url -> Api
apiFactory =
    Api


getApiClient : Api -> ApiClient msg
getApiClient api =
    { adminLogIn = adminSignin api
    , adminSignUp = adminSignup api
    , getAdminSession = getAdminSession api
    , getManySites = getSites api
    , getSite = getSingleSite api
    , registerSite = registerSite api
    }


urlToString : Url -> String
urlToString url =
    let
        raw =
            Url.toString url
    in
    if String.endsWith "/" raw then
        String.dropRight 1 raw

    else
        raw


makeRequestUrl : Api -> String -> String
makeRequestUrl (Api url) routePath =
    let
        adminPathRoot =
            "admins"

        routePathList =
            String.split "/" routePath

        pathComponents =
            adminPathRoot :: routePathList
    in
    Url.Builder.crossOrigin
        (urlToString url)
        pathComponents
        []


secureRequestFactory : Method -> Input.SessionToken -> RequestTemplate
secureRequestFactory method token =
    let
        rawMethod =
            case method of
                Get ->
                    "GET"

                Post ->
                    "POST"
    in
    { method = rawMethod
    , headers = [ Http.header "Authorization" token ]
    , tracker = Nothing
    , timeout = Nothing
    }


securePost :
    String
    -> Input.SessionToken
    -> Http.Body
    -> Http.Expect msg
    -> Cmd msg
securePost endpoint token body expect =
    let
        extraInfo =
            secureRequestFactory Post token
    in
    Http.request
        { method = extraInfo.method
        , url = endpoint
        , headers = extraInfo.headers
        , body = body
        , expect = expect
        , timeout = extraInfo.timeout
        , tracker = extraInfo.tracker
        }


secureGet :
    String
    -> Input.SessionToken
    -> Http.Expect msg
    -> Cmd msg
secureGet endpoint token expect =
    let
        extraInfo =
            secureRequestFactory Get token
    in
    Http.request
        { method = extraInfo.method
        , url = endpoint
        , headers = extraInfo.headers
        , body = Http.emptyBody
        , expect = expect
        , timeout = extraInfo.timeout
        , tracker = extraInfo.tracker
        }



-- Api Requests


type alias AdminSignUp msg =
    ToMsg Input.AdminWithToken msg -> Output.Signup -> Cmd msg


adminSignup : Api -> AdminSignUp msg
adminSignup api toMsg data =
    let
        signupJson =
            E.object
                [ ( "username", E.string data.username )
                , ( "email", E.string data.email )
                , ( "password", E.string data.password )
                , ( "passwordConfirm", E.string data.passwordConfirm )
                ]

        body =
            Http.jsonBody signupJson

        expect =
            Http.expectJson toMsg Input.adminAndTokenDecoder
    in
    Http.post
        { body = body
        , expect = expect
        , url = makeRequestUrl api "signup"
        }


type alias AdminLogIn msg =
    ToMsg Input.AdminWithToken msg -> Output.Signin -> Cmd msg


adminSignin : Api -> AdminLogIn msg
adminSignin api toMsg data =
    let
        signinJson =
            E.object
                [ ( "username", E.string data.username )
                , ( "password", E.string data.password )
                ]

        body =
            Http.jsonBody signinJson

        expect =
            Http.expectJson toMsg Input.adminAndTokenDecoder
    in
    Http.post
        { body = body
        , expect = expect
        , url = makeRequestUrl api "signin"
        }



-- Private Routes


type alias GetAdminSession msg =
    Input.SessionToken -> ToMsg Input.Admin msg -> Cmd msg


getAdminSession : Api -> GetAdminSession msg
getAdminSession api token toMsg =
    let
        expect =
            Http.expectJson toMsg (D.field "data" Input.adminDecoder)
    in
    secureGet
        (makeRequestUrl api "profile")
        token
        expect


type alias GetManySites msg =
    Input.SessionToken -> (RemoteData.WebData Input.Sites -> msg) -> Cmd msg


getSites : Api -> GetManySites msg
getSites api token toMsg =
    let
        sitesDecoder =
            D.field "data" (D.list Input.siteDecoder)

        expect =
            Http.expectJson (RemoteData.fromResult >> toMsg) sitesDecoder
    in
    secureGet
        (makeRequestUrl api "sites")
        token
        expect


type alias GetSite msg =
    Input.SessionToken -> String -> (RemoteData.WebData Input.Site -> msg) -> Cmd msg


getSingleSite : Api -> GetSite msg
getSingleSite api token siteId toMsg =
    let
        siteDecoder =
            D.field "data" Input.siteDecoder

        expect =
            Http.expectJson (RemoteData.fromResult >> toMsg) siteDecoder
    in
    secureGet
        (makeRequestUrl api "sites/" ++ siteId)
        token
        expect


type alias RegisterSite msg =
    Input.SessionToken -> ToMsg Input.Site msg -> Output.RegisterSite -> Cmd msg


registerSite : Api -> RegisterSite msg
registerSite api token toMsg data =
    let
        siteJson =
            E.object
                [ ( "hostname", E.string data.hostname )
                ]

        body =
            Http.jsonBody siteJson

        expect =
            Http.expectJson
                toMsg
                (D.field "data" Input.siteDecoder)
    in
    securePost
        (makeRequestUrl api "sites/register")
        token
        body
        expect
