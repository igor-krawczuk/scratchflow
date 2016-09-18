module GUI exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Window as Window
import Task
import Mouse exposing (Position)
import Dict
import Debug exposing (..)

-- IMPORT COMPONENTS
import Selector
import GraphicalNode
import GraphArea

import Tree
import Crawl

renderTop model = text ""

-- MODEL

type SubscriptionEvent =
    WindowResize Window.Size

type Msg= NoOp
    | SubscriptionUpdate SubscriptionEvent
    | SelectorUpdate Selector.Msg
    | GraphAreaUpdate GraphArea.Msg
    | CheckQueue

type alias SubData = {wsize: Window.Size}

type alias Model = {subs:SubData,
    selectorModel:Selector.Model,
    subQ:List Msg,
    graphAreaModel:GraphArea.Model
    }



-- STYLES
topStyle = [("height","100%")]

-- UPDATE
update: Msg->Model-> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model,Cmd.none)
        SubscriptionUpdate submsg -> handleSubs submsg model
        SelectorUpdate selmsg -> handleSelectorUpdate (Debug.log "GUI-selmsg" selmsg) model
        GraphAreaUpdate grapharemsg -> handleGraphAreaUpdate grapharemsg model
        CheckQueue -> handleQueue model






-- SUBSCRIPTIONS
subscriptions: Model -> Sub Msg
subscriptions model= Sub.batch [
    Window.resizes winSizeToMsg,
    Sub.map SelectorUpdate (Selector.subscriptions model.selectorModel),
    Sub.map GraphAreaUpdate (GraphArea.subscriptions model.graphAreaModel)
    ]

-- VIEW
view : Model -> Html Msg
view model= div [style topStyle] [renderTop model,
    wrappedSelector model,
    wrappedGraphArea model,
    displayCode model,
    displayTree model,
    displayVisGraph model
    ]
displayTree model=
    div [] [h3 [] [text "Nodes in DataFlow Graph model"],br [] [],text (toString model.graphAreaModel.graph)]
displayVisGraph model=
    div [] [h3 []  [text "Nodes in Visual model"],br [] [],text (toString model.graphAreaModel.nodes),
        h3[][text "Edges in Visual model"],br [] [],text (toString model.graphAreaModel.edges)
                       ]
displayCode:Model->Html Msg
displayCode model=
    div [] (
        [
            h3 [] [text "Generated Code"],
            br[][]
            ]++(
            List.map (\c -> p[] [pre [] [text c]]) (Crawl.crawlToList model.graphAreaModel.graph))
        )
wrappedSelector:Model-> Html Msg
wrappedSelector model =
    App.map SelectorUpdate (Selector.view model.selectorModel)

wrappedGraphArea:Model-> Html Msg
wrappedGraphArea model =
    App.map GraphAreaUpdate (GraphArea.view model.graphAreaModel)
-- Widget Handlers

-- HELPERS
winSizeToMsg: Window.Size -> Msg
winSizeToMsg size = SubscriptionUpdate (WindowResize size)

-- handlers

handleQueue:Model ->(Model,Cmd Msg)
handleQueue model=
        case List.head model.subQ of
            Nothing ->(model,Cmd.none)
            Just a-> 
            let t= case List.tail model.subQ  of 
                Nothing -> []
                Just ta -> ta 
            in let (newM, newcmd)= update a {model | subQ=t}
                           in handleQueue newM

handleSubs: SubscriptionEvent-> Model -> (Model, Cmd Msg)
handleSubs msg model = 
    case msg of
        WindowResize size-> resizeChilden model size

resizeChilden:Model->Window.Size->(Model,Cmd  Msg)
resizeChilden model size=
        let 
            oldq=model.subQ
            newmodel ={model| subs=updateWinDims model.subs size}
            newSelWidth =(0.2* toFloat size.width|> round)
        in
                    update CheckQueue {newmodel | subQ= (queueResize oldq newSelWidth)}

queueResize:List Msg->Int->List Msg
queueResize oldq newSelWidth=
    List.append oldq [
                    (SelectorUpdate (Selector.ChangeWidth newSelWidth)),
                    (GraphAreaUpdate (GraphArea.ChangeOffset newSelWidth))
                    ]

updateWinDims:SubData->Window.Size->SubData
updateWinDims subs size=
    {subs | wsize=size}

handleSelectorUpdate:Selector.Msg ->Model -> (Model,Cmd Msg)
handleSelectorUpdate selmsg model=
        let 
            (newselmodel,selcm,pmsg)= (Selector.update selmsg model.selectorModel)
            newModel ={model | selectorModel = newselmodel}
                 in 
                case (Debug.log"gui-Sendnodepmsg" pmsg) of
                    Just (Selector.SendNode node)-> handleGraphAreaUpdate (GraphArea.AddNode node) newModel
                    Nothing -> (newModel, Cmd.map SelectorUpdate selcm)


handleGraphAreaUpdate:GraphArea.Msg -> Model -> (Model,Cmd Msg)
handleGraphAreaUpdate grapharemsg model=
        let 
            (newgam,gacm, pmsg)= (GraphArea.update grapharemsg model.graphAreaModel)
            newModel = {model | graphAreaModel=newgam}
             in 
                case pmsg of
                    Nothing -> (newModel,Cmd.map GraphAreaUpdate gacm)
                    Just GraphArea.NodeReceived -> handleSelectorUpdate (Selector.ClearNode) newModel


helperGetInit =
    (Model (SubData (Window.Size 0 0 ))
    (
        Selector.Model 
        1 -- start with 1, since 0 is error value
        getOptions
        Nothing
    )
    []
    (GraphArea.Model Dict.empty 0 0 Tree.newTree Nothing Dict.empty)
    ,
    Task.perform (\_-> NoOp) winSizeToMsg Window.size)

getOptions =
        (List.map (\t ->Selector.Option (toString t) 0 t) nodeTypes )

tensorTypes=[
      Tree.FloatTensor
    , Tree.IntTensor
    , Tree.NumberTensor
    , Tree.BoolTensor
    , Tree.StringTensor
    , Tree.AnyTensor
    , Tree.NoTensor
    ]

nodeTypes= (List.map (\t-> Tree.Input (toString t) t) tensorTypes)++[ 
                Tree.Output
              --, Tree.Constant Tensor
              , Tree.Variable
              --, Tree.Zeros TensorType
              , Tree.Add
              , Tree.Sub
              , Tree.Mul
              , Tree.Div
              , Tree.Mod
              , Tree.Neg
              , Tree.Log
              , Tree.Equal
              --, Tree.Argmax Int
              --, Tree.Cast TensorType
              --, Tree.RandomNormal Float Float
              , Tree.MatMul
              , Tree.SoftMax
              , Tree.ReduceMean
              --, Tree.ReduceSum (List Int)
              --, Tree.TrainGDOMinimize Float
              ]
