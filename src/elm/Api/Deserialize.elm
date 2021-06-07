module Api.Deserialize exposing
    ( Admin
    , AdminWithToken
    , Author(..)
    , Comment
    , Comments
    , SessionToken
    , Site
    , Sites
    , adminAndTokenDecoder
    , adminDecoder
    , commentsDecoder
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


type Author
    = Anonymous String
    | Known String


type alias Post =
    { id : String
    }


type alias Comment =
    { id : String
    , body : String
    , author : Author
    , post : Post
    , createdAt : Time.Posix
    }


type alias Comments =
    List Comment


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


commentsDecoder : Decoder Comments
commentsDecoder =
    D.map4
        (\id anonAuthorName body createdAt ->
            { id = id
            , body = body
            , author = Anonymous anonAuthorName
            , createdAt = createdAt
            , post =
                { id = ""
                }
            }
        )
        (D.field "id" D.string)
        (D.field "anonAuthorName" D.string)
        (D.field "body" D.string)
        (D.field "createdAt" posixTimeDecoder)
        |> D.list
