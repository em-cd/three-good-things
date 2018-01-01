import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model =
  { entries : List Entry
  , input : String
  }

type alias Entry =
  { description : String }

model : Model
model =
  { entries = []
  , input = "" }

newEntry : String -> Entry
newEntry desc  =
  { description = desc
  }

-- UPDATE

type Msg
  = UpdateField String
  | Add

update : Msg -> Model -> Model
update msg model =
  case msg of
    Add ->
      { model
        | entries =
          if String.isEmpty model.input then
            model.entries
          else
            model.entries ++ [ newEntry model.input ]
      }
    UpdateField input ->
      { model | input = input }

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "wrapper"
    , style [ ("background-color", "blanchedalmond") ]
    ]
    [ section
      [ class "app"
      ]
      [ input
        [ placeholder "something good"
        , onInput UpdateField
        , onEnter Add
        ][]
        , viewEntries model.entries 
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

-- VIEW ENTRIES

viewEntries : List Entry -> Html Msg
viewEntries entries =
  section
    [ class "main" ]
    [ ul <|
      List.map
        [ li [] ]
    ]
