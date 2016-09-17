module GraphicalNode exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Task

-- MODEL


type alias Model =
    { position : Position
    , drag : Maybe Drag,
    parent:String
    }


type alias Drag =
    { start : Position
    , current : Position
    }


-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position
    | ReleasedAt Int Int Model
    | SetParent String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({position, drag} as model) =
  case msg of
    DragStart xy ->
      Model position (Just (Drag xy xy)) model.parent

    DragAt xy ->
      Model position (Maybe.map (\{start} -> Drag start xy) drag) model.parent

    DragEnd p ->
      let newmodel =Model (getPosition model) Nothing model.parent in
            let pos = getPosition model in
                      let (nm2,cm) =update (ReleasedAt pos.x pos.y newmodel) newmodel in
                                                                                nm2
    ReleasedAt x y n -> model
    SetParent p -> {model|parent=p}



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) = (,)

leftBarWidth=20
rightAreaWidth=100-leftBarWidth
leftBarStyle = style ["background-color"=>"red",
    "width"=> (toString leftBarWidth  ++ "%"),
    "float"=>"left",
    "border-right-width"=>"2px",
    "border-right-style"=>"solid",
    "height"=>"100%"]
rightAreaStyle = style ["background-color"=>"green",
    "margin-left"=> (toString leftBarWidth ++ "%"),
    "height"=>"100%"]

view : Model -> Html Msg
view model =
    let
        realPosition =
            getPosition model
        color="red"
    in
               div 
               [
                   onMouseDown
                   , style
                   [ "background-color" => color--"#3C8D2F"
                   , "cursor" => "move"

                   , "width" => "100px"
                   , "height" => "100px"
                   , "border-radius" => "4px"
                   , "position" => "absolute"
                   , "left" => px realPosition.x
                   , "top" => px realPosition.y

                   , "color" => "white"
                   , "display" => "flex"
                   , "align-items" => "center"
                   , "justify-content" => "center"
                   ]
               ]
               [ text ("Drag Me!" ++ toString realPosition.x ++" "++toString realPosition.y),
               text model.parent
               ]


px : Int -> String
px number =
  toString number ++ "px"


getPosition : Model -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)


onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)

---plan
--- write update/model/view architecture for node generation
