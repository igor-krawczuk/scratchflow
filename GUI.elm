import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Window as Window
import Task




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

type alias SubData = {wsize: Window.Size}
type alias Model = {subs:SubData}

-- STYLES
topStyle = [("height","100%")]

-- INIT
init = (
    Model (SubData (Window.Size 0 0 )),
    Task.perform (\_-> NoOp) winSizeToMsg Window.size)

winSizeToMsg: Window.Size -> Msg
winSizeToMsg size = SubscriptionUpdate (WindowResize size)

-- UPDATE
update: Msg->Model-> (Model, Cmd Msg)
update msg model =
    case msg of
        SubscriptionUpdate submsg -> handleSubs submsg model
        NoOp -> (model,Cmd.none)


handleSubs: SubscriptionEvent-> Model -> (Model, Cmd Msg)
handleSubs msg model = 
    case msg of
        WindowResize size-> ({model| subs=updateWinDims model.subs size},Cmd.batch [Cmd.none ])

updateWinDims: SubData->Window.Size->SubData
updateWinDims subdata size=
    {subdata|wsize=size}

-- SUBSCRIPTIONS
subscriptions: Model -> Sub Msg
subscriptions model= Sub.batch [ Window.resizes winSizeToMsg]


-- VIEW
view : Model -> Html Msg
view model= div [style topStyle] [renderTop model]

renderTop model = text (" width with 20% of " ++ toString model.subs.wsize.width)
