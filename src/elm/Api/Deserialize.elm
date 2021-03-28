module Api.Deserialize exposing
    ( Admin
    , AdminWithToken
    , SessionToken
    , Site
    , Sites
    , adminAndTokenDecoder
    , adminDecoder
    , siteDecoder
    )

import Json.Decode as D exposing (Decoder)
import Time



-- TODO: convert into opaque type


type alias SessionToken =
    String


type alias Admin =
    { id : String
    , username : String
    , created : Time.Posix
    , updated : Time.Posix
    }


type alias Site =
    { id : String
    , hostname : String
    , created : Time.Posix
    , updated : Time.Posix
    }


type alias Sites =
    List Site


type alias AdminWithToken =
    ( Admin, SessionToken )


adminDecoder : Decoder Admin
adminDecoder =
    D.map4 Admin
        (D.field "id" D.string)
        (D.field "username" D.string)
        (D.field "createdAt" posixTimeDecoder)
        (D.field "updatedAt" posixTimeDecoder)


adminAndTokenDecoder : Decoder ( Admin, SessionToken )
adminAndTokenDecoder =
    D.map2 Tuple.pair
        (D.field "data" adminDecoder)
        (D.field "sessionToken" D.string)


posixTimeDecoder : Decoder Time.Posix
posixTimeDecoder =
    D.int
        |> D.map Time.millisToPosix


siteDecoder : Decoder Site
siteDecoder =
    D.map4 Site
        (D.field "id" D.string)
        (D.field "hostname" D.string)
        (D.field "createdAt" posixTimeDecoder)
        (D.field "updatedAt" posixTimeDecoder)
