module Selector exposing (renderSelector,subscribeSelector, NodeSelector )
import Html exposing (..)
-- MY IMPORTS
import GraphicalNode exposing (GraphicalNode,FlowNode)
type alias Option = {text:String,nodeType:FlowNode}

type NewNodeStatus = NEW | READYTOADD| READYTODELETE
type alias NewNode = {node:GraphicalNode, status:NewNodeStatus}
-- MODEL
type alias StyleAttr= (String,String)
type alias NodeSelector = {style: List StyleAttr, options: List Option, newNode: Maybe NewNode}

type Msg = 

nodeSelStyle= [("height","100%")]

-- SUBSCRIPTION HOOK
subscribeSelector:NodeSelector-> List (Sub Msg)
subscribeSelector nodesel= [Sub.none]

-- VIEW HOOK
renderSelector:NodeSelector -> Html Msg
renderSelector nodesel= text "Here be renderSel"

renderNewNode:Maybe NewNode -> Html Msg
renderNewNode graphnode= text "Here be newNode " -- render as long as not release outside of selector, on release outside selector either delete or put into outbox
