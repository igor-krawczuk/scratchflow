module Tree exposing (..)

import Array
import List
import Maybe

type Tensor = Scalar Float | Vector (List Float) | Matrix (List (List Float)) | Cube (List (List (List Float)))
type TensorType = FloatTensor | IntTensor | NumberTensor | BoolTensor | StringTensor | AnyTensor

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

type NodeType = Input String TensorType | Output |
    Constant Tensor | Variable Tensor |
    Add | Sub | Mul | Div | Mod |
    RandomNormal Float Float

nbInputs : NodeType -> Int
nbInputs nodeType = List.length (inputTypes nodeType)

inputTypes : NodeType -> List TensorType
inputTypes nodeType = 
    case nodeType of
        Input _ _ -> []
        Output -> [AnyTensor]
        Constant _ -> []
        Variable _ -> []
        Add -> [NumberTensor, NumberTensor]
        Sub -> [NumberTensor, NumberTensor]
        Mul -> [NumberTensor, NumberTensor]
        Div -> [NumberTensor, NumberTensor]
        Mod -> [NumberTensor, NumberTensor]
        RandomNormal _ _ -> [IntTensor]

nbOutputs : NodeType -> Int
nbOutputs nodeType = List.length (outputTypes nodeType)

outputTypes : NodeType -> List TensorType
outputTypes nodeType =
    case nodeType of
        Input _ _ -> [AnyTensor]
        Output -> []
        Constant _ -> [AnyTensor]
        Variable _ -> [AnyTensor]
        Add -> [NumberTensor]
        Sub -> [NumberTensor]
        Mul -> [NumberTensor]
        Div -> [NumberTensor]
        Mod -> [NumberTensor]
        RandomNormal _ _ -> [FloatTensor]

compatibleTypes : TensorType -> TensorType -> Bool
compatibleTypes type1 type2 = case type1 of
    NumberTensor -> type2 == NumberTensor || type2 == IntTensor || type2 == FloatTensor || type2 == AnyTensor
    IntTensor -> type2 == IntTensor || type2 == NumberTensor || type2 == AnyTensor
    FloatTensor -> type2 == FloatTensor || type2 == NumberTensor || type2 == AnyTensor
    AnyTensor -> True
    _ -> type2 == type1 || type2 == AnyTensor

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