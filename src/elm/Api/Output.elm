{-
  This module defines the data that will get sent
  to the API Server
-}
module Api.Output exposing 
  ( Signup
  , Signin
  )


type alias Signup = 
  { username        : String
  , password        : String
  , passwordConfirm : String
  }

type alias Signin =
  { username : String
  , password : String
  }

