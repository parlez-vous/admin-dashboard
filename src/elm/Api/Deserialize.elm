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
      (D.at ["data", "id"] D.int)
      (D.at ["data", "username"] D.string)
      (D.at ["data", "created_at"] Iso8601.decoder)
      (D.at ["data", "updated_at"] Iso8601.decoder))

