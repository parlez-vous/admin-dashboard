module Api exposing
  ( adminSignup
  , adminSignin
  , getAdminSession
  )

import Http
import Json.Encode as E

import Api.Deserialize as D
import Api.Output as Output


api : String
api = "http://staging.api.parlez-vous.io/admins"

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


requestFactory : Method -> RequestTemplate
requestFactory method =
  let
    rawMethod =
      case method of
        Get  -> "GET"
        Post -> "POST"

  in
  { method  = rawMethod
  , headers = []
  , tracker = Nothing
  , timeout = Nothing
  }



post :
  String ->
  Http.Body ->
  Http.Expect msg ->
  Cmd msg
post endpoint body expect =
  let
    extraInfo = requestFactory Post
    
  in
  Http.request
    { method = extraInfo.method
    , url = api ++ endpoint
    , headers = extraInfo.headers
    , body = body
    , expect = expect
    , timeout = extraInfo.timeout
    , tracker = extraInfo.tracker
    }



get :
  String ->
  Http.Expect msg ->
  Cmd msg
get endpoint expect =
  let
    extraInfo = requestFactory Get
  in
  Http.request
    { method = extraInfo.method
    , url = api ++ endpoint
    , headers = extraInfo.headers
    , body = Http.emptyBody
    , expect = expect
    , timeout = extraInfo.timeout
    , tracker = extraInfo.tracker
    }


adminSignup : ToMsg D.AdminWithToken msg -> Output.Signup -> Cmd msg
adminSignup toMsg data =
  let
    signupJson = 
      E.object
        [ ( "username", E.string data.username )
        , ( "password", E.string data.password )
        , ( "passwordConfirm", E.string data.passwordConfirm )
        ]

    body = Http.jsonBody signupJson

    expect = Http.expectJson toMsg D.adminAndTokenDecoder

  in
    post "/signup" body expect



adminSignin : ToMsg D.AdminWithToken msg -> Output.Signin -> Cmd msg
adminSignin toMsg data =
  let
    signinJson =
      E.object
        [ ( "username", E.string data.username )
        , ( "password", E.string data.password )
        ]
    
    body = Http.jsonBody signinJson

    expect = Http.expectJson toMsg D.adminAndTokenDecoder

  in
    post "/signin" body expect


getAdminSession : ToMsg D.Admin msg -> Cmd msg
getAdminSession toMsg =
  let
    expect = Http.expectJson toMsg D.adminDecoder

  in
    get "/profile" expect

