module Api.Deserialize exposing
  ( Admin
  , adminDecoder
  )

import Iso8601
import Json.Decode as D exposing (Decoder)
import Time

type alias Admin =
  { id       : Int
  , username : String
  , created  : Time.Posix
  , updated  : Time.Posix
  }


adminDecoder : Decoder Admin
adminDecoder =
  D.field "data"
    (D.map4 Admin
      (D.field "id" D.int)
      (D.field "username" D.string)
      (D.field "created_at" Iso8601.decoder)
      (D.field "updated_at" Iso8601.decoder))

