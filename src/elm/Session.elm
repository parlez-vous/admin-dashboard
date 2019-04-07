module Session exposing (User(..))

import Api.Deserialize as Input

type User
  = Guest
  | Admin Input.AdminWithToken
