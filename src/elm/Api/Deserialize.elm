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

import Iso8601
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
        (D.field "created_at" Iso8601.decoder)
        (D.field "updated_at" Iso8601.decoder)


adminAndTokenDecoder : Decoder ( Admin, SessionToken )
adminAndTokenDecoder =
    D.map2 Tuple.pair
        (D.field "data" adminDecoder)
        (D.field "sessionToken" D.string)


siteDecoder : Decoder Site
siteDecoder =
    D.map4 Site
        (D.field "id" D.string)
        (D.field "hostname" D.string)
        (D.field "created_at" Iso8601.decoder)
        (D.field "updated_at" Iso8601.decoder)
