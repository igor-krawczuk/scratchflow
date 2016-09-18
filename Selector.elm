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
    | SelNodeUpdate GraphicalNode.Msg
    | AddNode GraphicalNode.Model
    | ClearNode

type OutMsg = SendNode GraphicalNode.Model

type alias Option = {text:String, id:Int}
type alias SelNode = Maybe GraphicalNode.Model

type alias Model = {width:Int, options: List Option, selNode:SelNode}

selectorStyle: Int -> List (String,String)
selectorStyle width = [
    ("width", (toString width ) ++"px"),
    ("float", "left"),
    ("background-color","white"),
    ("border-right","thick solid black"),
    ("height","100%")
    ]
listStyle = [("","")]
optionstyle= [("","")]

update: Msg->Model-> (Model, Cmd Msg, Maybe OutMsg)
update msg model=
    case msg of
        ChangeWidth w -> ({model|width = w},Cmd.none,Nothing)
        SelNodeUpdate gnmsg -> handleSelNode gnmsg model
        ClearNode -> ({model| selNode=Nothing},Cmd.none,Nothing)
        AddNode n -> ({model| selNode=Just n},Cmd.none,Nothing)



view : Model -> Html Msg
view model =
    div [style (selectorStyle model.width)]
    [ul [ style listStyle] (
        List.map (\o -> li [optionSpawn o.text, style optionstyle] [text o.text] ) model.options),
        renderSelNode model.selNode
        ]


selsubs=[]
subscriptions: Model -> Sub Msg
subscriptions model = case model.selNode of 
        Nothing -> Sub.batch ([ Sub.none]++selsubs)
        Just nn -> Sub.batch ([Sub.map SelNodeUpdate (GraphicalNode.subscriptions nn)]++selsubs)



-- HANDLERS
handleSelNode:GraphicalNode.Msg-> Model->(Model,Cmd Msg, Maybe OutMsg)
handleSelNode gnmsg model=
        case gnmsg of 
        GraphicalNode.DragStart pos id-> forwardMsg gnmsg model
        GraphicalNode.DragAt pos id-> forwardMsg gnmsg model
        GraphicalNode.DragEnd pos id-> forwardMsg gnmsg model
        GraphicalNode.SetParent par id-> forwardMsg gnmsg model
            
forwardMsg:GraphicalNode.Msg->Model->(Model,Cmd Msg,Maybe OutMsg)
forwardMsg gnmsg model=
            case model.selNode of
                    Just node -> let
                                    (newslm,slncm, gomsg)=GraphicalNode.update gnmsg node
                                in 
                                   case gomsg of
                                       Just (GraphicalNode.ReleasedAt x y id) ->checkNodeRelease x y id model
                                       Nothing->({model | selNode = Just newslm},Cmd.map SelNodeUpdate slncm,Nothing)
                    Nothing -> (model,Cmd.none, Nothing ) --check how this couzld happen..

checkNodeRelease:Int->Int->Int->Model->(Model,Cmd Msg,Maybe OutMsg)
checkNodeRelease x y id model =
       if x < model.width then ({model|selNode=Nothing},Cmd.none,Nothing) else
          case model.selNode of
                        Nothing -> (model,Cmd.none,Nothing)--check how this could be
                        Just nn -> ({model | selNode = Nothing},Cmd.none,Just (SendNode nn))

-- HELPERS



optionSpawn :String->Attribute Msg
optionSpawn text=
  Html.Events.on "mousedown" (Json.map (\p-> AddNode (GraphicalNode.Model p (Just(GraphicalNode.Drag p p))  text 0)) Mouse.position)


renderSelNode:SelNode -> Html Msg
renderSelNode newnode=
    case newnode of
        Nothing -> text ""
        Just node-> App.map SelNodeUpdate (GraphicalNode.view node)
