module Tree exposing (..)

import Array
import List
import Maybe

-- For now no edges, they are stored in the nodes (2-way linked list)
nodeId : Int
nodeId = 1

type Tensor = Scalar Float | Vector (List Float) | Matrix (List (List Float)) | Cube (List (List (List Float)))

type alias Tree = {
    nodes : List Node
}

type alias Node = {
    id : Int,
    nodeType : NodeType,
    inputs : Array.Array (Maybe Int),
    outputs : Array.Array (Maybe Int)
}

type NodeType = Input Tensor | Output | 
    Constant Tensor | Variable Tensor |
    AddType | SubType | MulType | DivType

nbInputs : NodeType -> Int
nbInputs nodeType = 
    case nodeType of
        Input _ -> 0
        Output -> 2
        Constant _ -> 0
        Variable _ -> 0
        AddType -> 2
        SubType -> 2
        MulType -> 2
        DivType -> 2

nbOutputs : NodeType -> Int
nbOutputs nodeType =
    case nodeType of
        Input _ -> 1
        Output -> 0
        Constant _ -> 1
        Variable _ -> 1
        AddType -> 1
        SubType -> 1
        MulType -> 1
        DivType -> 1

bindNodes : (Node, Int) -> (Node, Int) -> Tree -> Tree
bindNodes (node1, id1) (node2, id2) tree = let
        newNode1 = { node1 | outputs = Array.set id1 (Just node2.id) node1.outputs }
        newNode2 = { node2 | inputs = Array.set id2 (Just node1.id) node2.inputs }
        otherNodes = List.filter (\x -> x /= node1 && x /= node2) tree.nodes
        newNodes = List.append otherNodes [newNode1, newNode2]
    in { tree | nodes = newNodes }

unbindNodes : (Node, Int) -> (Node, Int) -> Tree -> Tree
unbindNodes (node1, id1) (node2, id2) tree = let
        newNode1 = { node1 | outputs = Array.set id1 (Nothing) node1.outputs }
        newNode2 = { node2 | inputs = Array.set id2 (Nothing) node2.inputs }
        otherNodes = List.filter (\x -> x /= node1 && x /= node2) tree.nodes
        newNodes = List.append otherNodes [newNode1, newNode2]
    in { tree | nodes = newNodes }

newTree : Tree
newTree = { nodes = [] }

addNode : NodeType -> Tree -> Tree
addNode nodeType tree = let
    nodeId = nodeId + 1
    in {
        tree | nodes = List.append tree.nodes [{
            id = nodeId,
            nodeType = nodeType,
            inputs = Array.repeat (nbInputs nodeType) Nothing,
            outputs = Array.repeat (nbOutputs nodeType) Nothing
        }]
    }

deleteNode : Node -> Tree -> Tree
deleteNode node tree = { tree | nodes = List.filter (\x -> x /= node) tree.nodes }