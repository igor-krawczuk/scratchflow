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
import Tree


type Msg = AddNode GraphicalNode.Model
    | NodeUpdate GraphicalNode.Msg
    | ChangeOffset Int

type OutMsg = NodeReceived

type alias Edge={source:Int,sink: Maybe Int}
type alias RealEdge={source:Int,sink:Int}
type alias Model= {nodes:Dict.Dict Int GraphicalNode.Model,
                   offset:Int,
                   cur_id:Int,
                   graph:Tree.Tree,
                   newEdge:Maybe Edge,
                   edges:Dict.Dict (Int,Int) RealEdge
                }

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
    GraphicalNode.StartEdge node_id -> let newmodel =checkEdge node_id model in forwardMsg gnmsg (Dict.get node_id model.nodes) newmodel
    GraphicalNode.NoOp -> forwardMsg gnmsg Nothing model


checkEdge:Int->Model->Model
checkEdge node_id model=
    case model.newEdge of
        Nothing->{model|newEdge=Just (Edge node_id Nothing)}
        Just edge-> 
            let s= case edge.sink of
                    Nothing -> node_Id
                    Just s ->s
                in case Tree.bindNodes edge.source s  model.graph of
                    Nothing -> model
                    Just newGraph -> let es=newEdge (edge.source,s) model.edges in
                            { model | graph = newGraph,newEdge=Nothing, }


newEdge:Dict (Int,Int) RealEdge->(Int,Int)->Dict (Int,Int) RealEdge
newEdge olde s_to_s =
    Dict.insert s_to_s edge RealEdge (s_to_s)

forwardMsg:GraphicalNode.Msg->Maybe GraphicalNode.Model->Model->(Model, Cmd Msg, Maybe OutMsg)
forwardMsg gnmsg may_node model=
    case may_node of 
        Nothing -> (model,Cmd.none, Nothing)
        Just node -> let (newslm,slncm, gomsg)=GraphicalNode.update gnmsg node in
                      let newnodes=(Dict.insert node.id newslm model.nodes) in 
                                       ({model | nodes = newnodes},Cmd.map NodeUpdate slncm,Nothing)

addNode:Model->GraphicalNode.Model->Model
addNode model nn=
    let newid=model.cur_id+1 
        newtree =Tree.addNode nn.nodeType newid model.graph --if testing later, check with case

        pos = GraphicalNode.getPosition nn
        ni = {nn|id=newid, position=pos, drag=Nothing}
        newnodes=(Dict.insert newid ni model.nodes)
    in
        (Debug.log "add node to graph" {model|nodes=newnodes,cur_id=newid,graph=newtree })

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
