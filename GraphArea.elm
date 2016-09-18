module GraphArea exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Task
import Dict

import Debug exposing (..)
-- IMPORT MYMODULES
import GraphicalNode


type Msg = AddNode GraphicalNode.Model
    | NodeUpdate GraphicalNode.Msg
    | ChangeOffset Int

type OutMsg = NodeReceived

type alias Model= {nodes:Dict.Dict Int GraphicalNode.Model, offset:Int, id:Int}

view : Model -> Html Msg
view model =
    div [style (graphAreaStyle model.offset)]
    (List.map (\n -> renderNode n) (Dict.values model.nodes))

update: Msg->Model-> (Model, Cmd Msg, Maybe OutMsg)
update msg model=
    case msg of
        ChangeOffset w -> ({model|offset = w},Cmd.none, Nothing)
        NodeUpdate newnodemodel -> handleNodeUpdate model newnodemodel
        AddNode newnode -> ((addNode model newnode), Cmd.none,Just NodeReceived)

handleNodeUpdate:Model->GraphicalNode.Msg->(Model,Cmd Msg,Maybe OutMsg)
handleNodeUpdate model gnmsg=
    case gnmsg of
    GraphicalNode.DragStart pos node_id-> forwardMsg gnmsg (Dict.get node_id model.nodes) model
    GraphicalNode.DragAt pos node_id-> forwardMsg gnmsg (Dict.get node_id model.nodes) model
    GraphicalNode.DragEnd pos node_id-> forwardMsg gnmsg (Dict.get node_id model.nodes) model
    GraphicalNode.SetParent par node_id-> forwardMsg gnmsg (Dict.get node_id model.nodes) model

forwardMsg:GraphicalNode.Msg->Maybe GraphicalNode.Model->Model->(Model, Cmd Msg, Maybe OutMsg)
forwardMsg gnmsg may_node model=
    case may_node of 
        Nothing -> (model,Cmd.none, Nothing)
        Just node -> let (newslm,slncm, gomsg)=GraphicalNode.update gnmsg node in
                      let newnodes=(Dict.insert node.id newslm model.nodes) in 
                                       ({model | nodes = newnodes},Cmd.map NodeUpdate slncm,Nothing)

addNode:Model->GraphicalNode.Model->Model
addNode model nn=
    let newid=model.id+1 
        pos = GraphicalNode.getPosition nn
                in 
    let ni=(Debug.log"grapharea added" {nn|id=newid, position=pos, drag=Nothing}) in
    let newnodes=(Debug.log "grapharea newdict" (Dict.insert newid ni model.nodes)) in
            {model|nodes=newnodes,id=newid }

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
    ("width","100%"),
    ("height","100%")
    ]

optionSpawn : Attribute Msg
optionSpawn =
  Html.Events.onClick (AddNode makeNode)

makeNode:GraphicalNode.Model
makeNode=(GraphicalNode.Model (Position 5 10) Nothing  "graphc:" 0)
