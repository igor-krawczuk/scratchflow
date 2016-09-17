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



type alias SubData = {wsize: Window.Size}
type alias Model = {subs:SubData, selectorModel:Selector.Model}

-- STYLES
topStyle = [("height","100%")]

-- INIT
init = (
    Model (SubData (Window.Size 0 0 )) (Selector.Model 0 [Selector.Option "test" 0] (Just (Selector.NewNode Selector.PENDING (GraphicalNode.Model (Position 0 0) Nothing)))),
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

handleSelectorUpdate:Selector.Msg -> Model -> (Model,Cmd Msg)
handleSelectorUpdate selmsg model=
    let (newselmodel,selcm)= (Selector.update selmsg model.selectorModel)
                                 in ({model | selectorModel = newselmodel},Cmd.none)


handleSubs: SubscriptionEvent-> Model -> (Model, Cmd Msg)
handleSubs msg model = 
    case msg of
        WindowResize size-> let newmodel ={model| subs=updateWinDims model.subs size}in
                                update (SelectorUpdate (Selector.ChangeWidth size.width)) newmodel

updateWinDims: SubData->Window.Size->SubData
updateWinDims subdata size=
    {subdata|wsize=size}

-- SUBSCRIPTIONS
subscriptions: Model -> Sub Msg
subscriptions model= Sub.batch [ Window.resizes winSizeToMsg,Sub.map SelectorUpdate (Selector.subscriptions model.selectorModel) ]

-- VIEW
view : Model -> Html Msg
view model= div [style topStyle] [renderTop model, App.map SelectorUpdate (Selector.view model.selectorModel)]

renderTop model = text (" width with 20% of " ++ toString model.subs.wsize.width)
