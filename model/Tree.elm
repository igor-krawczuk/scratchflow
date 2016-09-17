module Tree exposing (..)

import Array
import List
import Maybe

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

dummyNode : Node
dummyNode = {
    id = 0,
    nodeType = Output,
    inputs = Array.fromList [],
    outputs = Array.fromList []
 }

type NodeType = Input Tensor | Output | 
    Constant Tensor | Variable Tensor |
    AddType | SubType | MulType | DivType

nbInputs : NodeType -> Int
nbInputs nodeType = 
    case nodeType of
        Input _ -> 0
        Output -> 1
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

bindNodes : (Int, Int) -> (Int, Int) -> Tree -> Tree
bindNodes (nodeId1, id1) (nodeId2, id2) tree = let
        node1 = case List.head (List.filter (\x -> x.id == nodeId1) tree.nodes) of
            Just n1 -> n1
            Nothing -> dummyNode
        node2 = case List.head (List.filter (\x -> x.id == nodeId2) tree.nodes) of
            Just n2 -> n2
            Nothing -> dummyNode
        newNode1 = { node1 | outputs = Array.set id1 (Just node2.id) node1.outputs }
        newNode2 = { node2 | inputs = Array.set id2 (Just node1.id) node2.inputs }
        otherNodes = List.filter (\x -> x.id /= node1.id && x.id /= node2.id) tree.nodes
        newNodes = List.append otherNodes [newNode1, newNode2]
    in { tree | nodes = newNodes }

unbindNodes : (Node, Int) -> (Node, Int) -> Tree -> Tree
unbindNodes (node1, id1) (node2, id2) tree = let
        newNode1 = { node1 | outputs = Array.set id1 (Nothing) node1.outputs }
        newNode2 = { node2 | inputs = Array.set id2 (Nothing) node2.inputs }
        otherNodes = List.filter (\x -> x.id /= node1.id && x.id /= node2.id) tree.nodes
        newNodes = List.append otherNodes [newNode1, newNode2]
    in { tree | nodes = newNodes }

newTree : Tree
newTree = { nodes = [] }

addNode : NodeType -> Int -> Tree -> Tree
addNode nodeType nodeId tree = { tree | nodes = List.append tree.nodes [{
        id = nodeId,
        nodeType = nodeType,
        inputs = Array.repeat (nbInputs nodeType) Nothing,
        outputs = Array.repeat (nbOutputs nodeType) Nothing
    }] }

deleteNode : Node -> Tree -> Tree
deleteNode node tree = { tree | nodes = List.filter (\x -> x /= node) tree.nodes }