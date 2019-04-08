module Api exposing
  ( adminSignup
  , adminSignin
  , getAdminSession
  )

import Http
import Json.Encode as E
import Json.Decode as D exposing (Decoder)

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


adminSignup : String -> ToMsg Input.AdminWithToken msg -> Output.Signup -> Cmd msg
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
      , url = api ++ "/signup"
      }



adminSignin : String -> ToMsg Input.AdminWithToken msg -> Output.Signin -> Cmd msg
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
      , url = api ++ "/signin"
      }
    




-- Private Routes

getAdminSession : Input.SessionToken -> String -> ToMsg Input.Admin msg -> Cmd msg
getAdminSession token api toMsg =
  let
    expect = Http.expectJson toMsg (D.field "data" Input.adminDecoder)

  in
    secureGet
      (api ++ "/profile")
      token 
      expect

