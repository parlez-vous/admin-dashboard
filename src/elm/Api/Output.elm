{-
   This module defines the data that will get sent
   to the API Server
-}


module Api.Output exposing
    ( RegisterSite
    , Signin
    , Signup
    )


type alias Signup =
    { username : String
    , email : String
    , password : String
    , passwordConfirm : String
    }


type alias Signin =
    { username : String
    , password : String
    }


type alias RegisterSite =
    { hostname : String
    }
