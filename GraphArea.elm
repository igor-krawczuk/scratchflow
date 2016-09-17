module GraphArea exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Task


-- IMPORT MYMODULES
import GraphicalNode


type Msg = AddNode GraphicalNode.Model
    | NodeUpdate GraphicalNode.Msg
    | ChangeOffset Int

type alias Model= {nodes:List GraphicalNode.Model, offset:Int}

view : Model -> Html Msg
view model =
    div [style (graphAreaStyle model.offset)]
    ([text "Grapharea"]++(List.map (\n -> renderNode n) model.nodes))

update: Msg->Model-> (Model, Cmd Msg)
update msg model=
    case msg of
        ChangeOffset w -> ({model|offset = w},Cmd.none)
        NodeUpdate newnodemodel -> (model,Cmd.none)
        AddNode newnode -> (addNode model newnode, Cmd.none)

addNode:Model->GraphicalNode.Model->Model
addNode model nn=
    let oldnodes=model.nodes in
            {model|nodes=List.append oldnodes [nn]}

renderNode:GraphicalNode.Model-> Html msg
renderNode node = text "muh"
        

graphareasubs = []
subscriptions: Model -> Sub Msg
subscriptions model = 
    Sub.batch ((List.map (\n-> Sub.map NodeUpdate (GraphicalNode.subscriptions n)) model.nodes)++graphareasubs)


graphAreaStyle:Int -> List(String,String)
graphAreaStyle offset =[
    ("margin-left", (toString offset) ++"px"),
    ("background-color","green"),
    ("width","100%")
    ]

