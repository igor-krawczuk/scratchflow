module GraphArea exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Task
import Dict


-- IMPORT MYMODULES
import GraphicalNode


type Msg = AddNode GraphicalNode.Model
    | NodeUpdate GraphicalNode.Msg
    | ChangeOffset Int

type alias Model= {nodes:Dict.Dict Int GraphicalNode.Model, offset:Int, id:Int}

view : Model -> Html Msg
view model =
    div [style (graphAreaStyle model.offset)]
    ([text "Grapharea"]++(List.map (\n -> renderNode n) (Dict.values model.nodes)))

update: Msg->Model-> (Model, Cmd Msg)
update msg model=
    case msg of
        ChangeOffset w -> ({model|offset = w},Cmd.none)
        NodeUpdate newnodemodel -> (model,Cmd.none)
        AddNode newnode -> ((addNode model newnode), Cmd.none)

addNode:Model->GraphicalNode.Model->Model
addNode model nn=
    let newid=model.id+1 in 
    let newnodes=(Dict.insert newid nn model.nodes) in
            {model|nodes=newnodes,id=newid}

renderNode:GraphicalNode.Model-> Html Msg
renderNode node = App.map NodeUpdate (GraphicalNode.view node)

graphareasubs = []
subscriptions: Model -> Sub Msg
subscriptions model = 
    Sub.batch ((List.map (\n-> Sub.map NodeUpdate (GraphicalNode.subscriptions n)) (Dict.values model.nodes))++graphareasubs)


graphAreaStyle:Int -> List(String,String)
graphAreaStyle offset =[
    ("margin-left", (toString offset) ++"px"),
    ("background-color","green"),
    ("width","100%")
    ]

