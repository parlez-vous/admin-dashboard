module Api exposing
  ( adminSignup
  , adminSignin
  , getAdminSession
  )

import Http
import Json.Encode as E

import Api.Deserialize as D
import Api.Output as Output


api = "http://staging.api.parlez-vous.io/admins"

type alias ToMsg a msg = (Result Http.Error a -> msg)

post :
  String ->
  Http.Body ->
  Http.Expect msg ->
  Cmd msg
post endpoint body expect = 
  Http.post
    { url = api ++ endpoint
    , body = body
    , expect = expect
    }


get :
  String ->
  Http.Expect msg ->
  Cmd msg
get endpoint expect =
  Http.get
    { url = api ++ endpoint
    , expect = expect
    }


adminSignup : ToMsg D.Admin msg -> Output.Signup -> Cmd msg
adminSignup toMsg data =
  let
    signupJson = 
      E.object
        [ ( "username", E.string data.username )
        , ( "password", E.string data.password )
        , ( "passwordConfirm", E.string data.passwordConfirm )
        ]

    body = Http.jsonBody signupJson

    expect = Http.expectJson toMsg D.adminDecoder

  in
    post "/signup" body expect



adminSignin : ToMsg D.Admin msg -> Output.Signin -> Cmd msg
adminSignin toMsg data =
  let
    signinJson =
      E.object
        [ ( "username", E.string data.username )
        , ( "password", E.string data.password )
        ]
    
    body = Http.jsonBody signinJson

    expect = Http.expectJson toMsg D.adminDecoder

  in
    post "/signin" body expect


getAdminSession : ToMsg D.Admin msg -> Cmd msg
getAdminSession toMsg =
  let
    expect = Http.expectJson toMsg D.adminDecoder

  in
    get "/profile" expect

