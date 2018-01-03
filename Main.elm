import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Date exposing (..)
import Task exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }

-- MODEL

type alias Model =
  { entries : List String
  , input : String
  , date : Maybe Date
  }

init : ( Model, Cmd Msg )
init =
  ( { entries = []
    , input = ""
    , date = Nothing
    }
  , Cmd.none
  )

-- UPDATE

type Msg
  = UpdateField String
  | Add
  | RequestDate
  | ReceiveDate Date

update msg model =
  case msg of
    Add ->
      ({ model
        | input = ""
        , entries =
          if String.isEmpty model.input then
            model.entries
          else
            model.entries ++ [ model.input ]
      }
      , Cmd.none )
    UpdateField input ->
      ({ model | input = input }
      , Cmd.none
      )
    
    RequestDate ->
      ( model, Task.perform ReceiveDate Date.now )

    ReceiveDate date ->
      let
        nextModel =
          { model | date = Just date }
      in
         ( nextModel, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "wrapper" ]
    [ section
      [ class "app" ]
      [ h1 [] [ text "three good things" ]
      , input
        [ placeholder "had a nice bath"
        , value model.input
        , onInput UpdateField
        , onEnter Add
        ]
        []
      , ol
        []
        ( List.map (\l -> li [] [ text l ]) model.entries )
      , button
        [ onClick RequestDate ] [ text "Request date" ]
      , text (getFormattedDate model.date)
      ]
    ]

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    isEnter code =
      if code == 13 then
       Json.succeed msg
     else
       Json.fail "not ENTER"
  in
    on "keydown" (Json.andThen isEnter keyCode)

getFormattedDate : Maybe Date -> String
getFormattedDate date =
  case date of
    Just d ->
      toString d
    Nothing ->
      "No date yet."
