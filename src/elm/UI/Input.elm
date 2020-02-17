module UI.Input exposing
  ( input
  , withLabel
  , withNoAutocomplete
  , withPlaceholder
  , toHtml
  , InputType(..)
  )

import Html exposing (Html, div, label, text)
import Html.Attributes as A exposing (class, type_)
import Html.Events as E

import Utils


type Input msg = Input (Options msg)

-- http://html5doctor.com/html5-forms-input-types/
type InputType msg
  = Text String (String -> msg)
  | Password String (String -> msg)
  | Email String (String -> msg)
  | Url String (String -> msg)
  | Checkbox Bool (Bool -> msg)


type alias Label =
  { identifier : String
  , value : String
  }


type alias Options msg =
  { disabled  : Bool
  , inputType : InputType msg
  , label : Maybe Label
  , placeholder : Maybe String
  , autocomplete : Bool
  }


defaultOptions : InputType msg -> Options msg
defaultOptions inputType =
  { disabled = False
  , autocomplete = True
  , inputType = inputType
  , label = Nothing
  , placeholder = Nothing
  }


input : InputType msg -> Input msg
input inputType =
  Input (defaultOptions inputType)



withLabel : String -> String -> Input msg -> Input msg
withLabel value for (Input opts) =
  let
    label =
      { identifier = for
      , value = value
      }
      
    newOpts = { opts | label = Just label }
  in
    Input newOpts


withNoAutocomplete : Input msg -> Input msg
withNoAutocomplete (Input opts) =
  Input { opts | autocomplete = False }




withPlaceholder : String -> Input msg -> Input msg 
withPlaceholder placeholder (Input opts) =
  let
    newOpts = { opts | placeholder = Just placeholder }
  in
    Input newOpts



toHtml : Input msg -> Html msg
toHtml (Input opts) =
  let
    inputClasses = Utils.toClass
      [ "border"
      , "border-solid"
      , "border-gray-400"
      , "rounded"
      , "px-4"
      , "py-2"
      , "w-64"
      , "block"
      , "bg-gray-200"
      , "focus:bg-gray-100"
      , "focus:outline-none"
      , "text-gray-700"
      , "leading-tight"
      ]

    (typeStr, inputValue, eventAction) =
      case opts.inputType of
        Checkbox checked action ->
          ("checkbox", A.checked checked, E.onCheck action)

        Text val action ->
          ("text", A.value val, E.onInput action)

        Password val action ->
          ("password", A.value val, E.onInput action)

        Email val action ->
          ("email", A.value val, E.onInput action)

        Url val action ->
          ("url", A.value val, E.onInput action)

    placeHolder =
      case opts.placeholder of
        Just str -> [ A.placeholder str ]
        Nothing  -> []
    
      
    (labelEl, inputId) =
      case opts.label of
        Just { identifier, value } ->
          let
            labelClasses = Utils.toClass
              [ "w-5/12"
              , "md:pr-6"
              , "text-gray-500"
              , "font-semibold"
              ]

            labelElement =
              label
                [ A.for identifier
                , labelClasses
                ]
                [ text value ]
          in
            ( [ labelElement ], [ A.id identifier ] )
        Nothing -> ([], [])

    inputAttributes =
      [ inputClasses
      , A.autocomplete opts.autocomplete
      , A.type_ typeStr
      , inputValue
      , eventAction
      ]
      |> List.append placeHolder
      |> List.append inputId

    contents =
      List.append labelEl [ Html.input inputAttributes [] ]
  in
    div [ class "md:flex md:items-center mt-3 mb-3" ] contents
