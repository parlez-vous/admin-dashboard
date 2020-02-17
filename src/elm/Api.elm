module Api exposing
  ( adminSignup
  , adminSignin
  , getAdminSession
  , getSites
  , getSingleSite
  , registerSite
  )

import Http
import Json.Encode as E
import Json.Decode as D
import RemoteData

import Api.Deserialize as Input
import Api.Output as Output


type alias ToMsg a msg = (Result Http.Error a -> msg)

type alias RequestTemplate =
  { method  : String
  , headers : List Http.Header
  , tracker : Maybe String
  , timeout : Maybe Float
  }

type Method
  = Get
  | Post



adminPath : String
adminPath = "/admins"


secureRequestFactory : Method -> Input.SessionToken -> RequestTemplate
secureRequestFactory method token =
  let
    rawMethod =
      case method of
        Get  -> "GET"
        Post -> "POST"

  in
  { method  = rawMethod
  , headers = [ Http.header "Authorization" token ]
  , tracker = Nothing
  , timeout = Nothing
  }



securePost :
  String ->
  Input.SessionToken ->
  Http.Body ->
  Http.Expect msg ->
  Cmd msg
securePost endpoint token body expect =
  let
    extraInfo = secureRequestFactory Post token
    
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
  String ->
  Input.SessionToken ->
  Http.Expect msg ->
  Cmd msg
secureGet endpoint token expect =
  let
    extraInfo = secureRequestFactory Get token

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


adminSignup : String -> ToMsg Input.AdminWithToken msg -> Output.Signup a -> Cmd msg
adminSignup api toMsg data =
  let
    signupJson = 
      E.object
        [ ( "username", E.string data.username )
        , ( "password", E.string data.password )
        , ( "passwordConfirm", E.string data.passwordConfirm )
        ]

    body = Http.jsonBody signupJson

    expect = Http.expectJson toMsg Input.adminAndTokenDecoder

  in
    Http.post
      { body = body
      , expect = expect
      , url = api ++ adminPath ++ "/signup"
      }



adminSignin : String -> ToMsg Input.AdminWithToken msg -> Output.Signin a -> Cmd msg
adminSignin api toMsg data =
  let
    signinJson =
      E.object
        [ ( "username", E.string data.username )
        , ( "password", E.string data.password )
        ]
    
    body = Http.jsonBody signinJson

    expect = Http.expectJson toMsg Input.adminAndTokenDecoder

  in
    Http.post
      { body = body
      , expect = expect
      , url = api ++ adminPath ++ "/signin"
      }
    




-- Private Routes

getAdminSession : Input.SessionToken -> String -> ToMsg Input.Admin msg -> Cmd msg
getAdminSession token api toMsg =
  let
    expect = Http.expectJson toMsg (D.field "data" Input.adminDecoder)

  in
    secureGet
      (api ++ adminPath ++ "/profile")
      token 
      expect



getSites :
  Input.SessionToken ->
  String ->
  (RemoteData.WebData Input.Sites -> msg) -> 
  Cmd msg
getSites token api toMsg =
  let
    sitesDecoder = D.field "data" (D.list Input.siteDecoder)

    expect = Http.expectJson (RemoteData.fromResult >> toMsg) sitesDecoder
  in
    secureGet
      (api ++ adminPath ++ "/sites")
      token
      expect  


getSingleSite :
  Input.SessionToken ->
  String ->
  String ->
  (RemoteData.WebData Input.Site -> msg) ->
  Cmd msg
getSingleSite token api siteId toMsg =
  let
    siteDecoder = D.field "data" Input.siteDecoder

    expect = Http.expectJson (RemoteData.fromResult >> toMsg) siteDecoder
  in
    secureGet
      (api ++ adminPath ++ "/sites/" ++ siteId)
      token
      expect



registerSite : 
  Input.SessionToken -> 
  String -> 
  ToMsg Input.Site msg -> 
  Output.RegisterSite ->
  Cmd msg
registerSite token api toMsg data =
  let
    siteJson =
      E.object
        [ ( "hostname", E.string data.hostname )
        ]
    
    body = Http.jsonBody siteJson

    expect =
      Http.expectJson
      toMsg
      (D.field "data" Input.siteDecoder)

  in
    securePost 
      (api ++ adminPath ++ "/sites/register")
      token
      body
      expect
    