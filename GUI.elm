import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Window as Window
import Task
import Mouse exposing (Position)

-- IMPORT COMPONENTS
import Selector
import GraphicalNode
import GraphArea

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

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

-- INIT
init = (
    Model (SubData (Window.Size 0 0 ))
    (
        Selector.Model 
        0 
        [Selector.Option "test" 0] 
        (Just (Selector.NewNode Selector.PENDING (GraphicalNode.Model (Position 0 0) Nothing)))
    )
    []
    (GraphArea.Model [] 0)
    ,
    Task.perform (\_-> NoOp) winSizeToMsg Window.size)

winSizeToMsg: Window.Size -> Msg
winSizeToMsg size = SubscriptionUpdate (WindowResize size)

-- UPDATE
update: Msg->Model-> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model,Cmd.none)
        SubscriptionUpdate submsg -> handleSubs submsg model
        SelectorUpdate selmsg -> handleSelectorUpdate selmsg model
        GraphAreaUpdate grapharemsg -> handleGraphAreaUpdate grapharemsg model
        CheckQueue -> handleQueue model

handleQueue:Model ->(Model,Cmd Msg)
handleQueue model=
        case List.head model.subQ of
            Nothing ->(model,Cmd.none)
            Just a->let t= List.tail model.subQ in 
                                                   let (newM, newcmd)= update a {model | subQ= (
                                                       case t of
                                                           Nothing->[]
                                                           Just ta->ta)}
                                                   in handleQueue newM
                                                      --case newcmd of
                                                      -- _->handleQueue newM--if we have a cmd with side effects, we do it here. else just go through the queueue

handleSelectorUpdate:Selector.Msg -> Model -> (Model,Cmd Msg)
handleSelectorUpdate selmsg model=
    let (newselmodel,selcm)= (Selector.update selmsg model.selectorModel)
                                 in ({model | selectorModel = newselmodel},Cmd.none)

handleGraphAreaUpdate:GraphArea.Msg -> Model -> (Model,Cmd Msg)
handleGraphAreaUpdate grapharemsg model=
    let (newgam,gacm)= (GraphArea.update grapharemsg model.graphAreaModel)
                                 in ({model | graphAreaModel = newgam},Cmd.none)


handleSubs: SubscriptionEvent-> Model -> (Model, Cmd Msg)
handleSubs msg model = 
    case msg of
        WindowResize size-> let newmodel ={model| subs=updateWinDims model.subs size}in
                                let oldq=newmodel.subQ in
                                update CheckQueue {newmodel | subQ=List.append oldq [
                                    (SelectorUpdate (Selector.ChangeWidth (0.2* toFloat size.width|> round))),
                                    (GraphAreaUpdate (GraphArea.ChangeOffset ((0.2* toFloat size.width|> round))))
                                    ]}

updateWinDims: SubData->Window.Size->SubData
updateWinDims subdata size=
    {subdata|wsize=size}

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
    App.map SelectorUpdate (Selector.view model.selectorModel),
    App.map GraphAreaUpdate (GraphArea.view model.graphAreaModel)
    ]

renderTop model = text ""
