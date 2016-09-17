module GraphicalNode exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Task

-- MODEL


type alias Model =
    { position : Position,
     drag : Maybe Drag,
        parent:String,
        id:Int
    }


type alias Drag =
    { start : Position
    , current : Position
    }


-- UPDATE

type OutMsg=ReleasedAt Int Int Int

type Msg
    = DragStart Position Int
    | DragAt Position Int
    | DragEnd Position Int
    | SetParent String Int


update : Msg -> Model -> ( Model, Cmd Msg,Maybe OutMsg )
update msg ({position, drag,parent,id} as model) =
  let 
      outmsg=case  msg of 
          DragEnd xy id-> let pos=(getPosition model)in Just (ReleasedAt pos.x pos.y id)
          _-> Nothing
      newmodel= case (Debug.log "node msg" msg) of
        DragStart xy id->
          Model position (Just (Drag xy xy)) parent id

        DragAt xy id->
          Model position (Maybe.map (\{start} -> Drag start xy) drag) parent id

        DragEnd p id->
          Model (getPosition model) Nothing parent id
        SetParent p id-> {model|parent=p}
  in (newmodel,Cmd.none,outmsg)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves (\p ->DragAt  p model.id) , Mouse.ups (\p -> DragEnd p model.id)]



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
                   onMouseDown model.id
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


onMouseDown : Int->Attribute Msg
onMouseDown id =
  on "mousedown" (Json.map (\p->DragStart p id) Mouse.position )

---plan
--- write update/model/view architecture for node generation
