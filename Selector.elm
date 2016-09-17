module Selector exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Task


-- IMPORT MYMODULES
import GraphicalNode

type Msg = ChangeWidth Int
    | NewNodeUpdate GraphicalNode.Msg

type alias Option = {text:String, id:Int}
type NewNodeStatus = PENDING | READYTODELETE | READYTOADD
type alias NewNode = {status:NewNodeStatus,node:GraphicalNode.Model}

type alias Model = {width:Int, options: List Option, newNode:Maybe NewNode}

selectorStyle: Int -> List (String,String)
selectorStyle width = [
    ("width", (toString width ) ++"px"),
    ("float", "left"),
    ("background-color","red")
    ]
listStyle = [("","")]
optionstyle= [("","")]

update: Msg->Model-> (Model, Cmd Msg)
update msg model=
    case msg of
        ChangeWidth w -> ({model|width = w},Cmd.none)
        NewNodeUpdate gnmsg -> handleNewNode gnmsg model

handleNewNode:GraphicalNode.Msg-> Model->(Model,Cmd Msg)
handleNewNode gnsmg model=
    let oldnn=model.newNode in
                       case oldnn of
                        Nothing -> (model,Cmd.none)--check how this could be
                        Just nn -> let (newNNmodel,nncm)= (GraphicalNode.update gnsmg nn.node)
                                   in( updateNN model newNNmodel,Cmd.none)

updateNN:Model->GraphicalNode.Model->Model
updateNN model nnm=
    case model.newNode of 
    Nothing ->let nn=(NewNode PENDING nnm) in {model|newNode =Just nn}
    Just oldnn->let nn={oldnn| node=nnm} in {model|newNode =Just nn}

view : Model -> Html Msg
view model =
    div [style (selectorStyle model.width)]
    [ul [ style listStyle] (
        List.map (\o -> li [ style optionstyle] [text o.text] ) model.options),
        renderNewNode model.newNode
        ]

renderNewNode:Maybe NewNode -> Html Msg
renderNewNode newnode=
    case newnode of
        Nothing -> text ""
        Just newn-> App.map NewNodeUpdate (GraphicalNode.view newn.node)

selsubs=[]
subscriptions: Model -> Sub Msg
subscriptions model = case model.newNode of 
        Nothing -> Sub.batch ([ Sub.none]++selsubs)
        Just nn -> Sub.batch ([Sub.map NewNodeUpdate (GraphicalNode.subscriptions nn.node)]++selsubs)
