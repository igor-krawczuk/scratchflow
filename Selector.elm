module Selector exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Task

type Msg = ChangeWidth Int

type alias Option = {text:String, id:Int}
type NewNodeStatus = READYTODELETE | READYTOADD
type alias NewNode = {status:NewNodeStatus,node:Int}

type alias Model = {width:Int, options: List Option, newNode:Maybe NewNode}

selectorStyle: Int -> List (String,String)
selectorStyle width = [
    ("width", ((0.2 * toFloat width) |> toString) ++"px"),
    ("background-color","red")
    ]
listStyle = [("","")]
optionstyle= [("","")]
update: Msg->Model-> (Model, Cmd Msg)
update msg model=
    case msg of
        ChangeWidth w -> ({model|width = w},Cmd.none)

view : Model -> Html Msg
view model =
    div [style (selectorStyle model.width)]
    [ul [ style listStyle] (
        List.map (\o -> li [ style optionstyle] [text o.text] ) model.options)
        ]
