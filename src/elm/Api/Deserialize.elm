module Api.Deserialize exposing
  ( Admin
  , adminDecoder
  , adminAndTokenDecoder
  , SessionToken
  , AdminWithToken
  )

import Iso8601
import Json.Decode as D exposing (Decoder)
import Time

type alias SessionToken = String

type alias Admin =
  { id       : Int
  , username : String
  , created  : Time.Posix
  , updated  : Time.Posix
  }

type alias AdminWithToken = (Admin, SessionToken)


adminDecoder : Decoder Admin
adminDecoder =
  (D.map4 Admin
    (D.field "id" D.int)
    (D.field "username" D.string)
    (D.field "created_at" Iso8601.decoder)
    (D.field "updated_at" Iso8601.decoder))


adminAndTokenDecoder : Decoder (Admin, SessionToken)
adminAndTokenDecoder =
  D.map2 Tuple.pair
    (D.field "data" adminDecoder)
    (D.field "sessionToken" D.string)
