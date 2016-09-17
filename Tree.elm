module Tree exposing (..)

import Array exposing (Array)

type Tensor = Scalar Float | Vector (List Float) | Matrix (List (List Float)) | Cube (List (List (List Float)))
type TensorType = FloatTensor | IntTensor | NumberTensor | BoolTensor | StringTensor | AnyTensor | NoTensor

type alias Tree = {
    nodes : List Node
}

type alias Node = {
    id : Int,
    nodeType : NodeType,
    inputs : Array.Array (Maybe Int),
    outputs : Array.Array (Maybe Int)
}

-- Node supposed to raise an error in bindNodes (no to be used in practice)
dummyNode : Node
dummyNode = {
    id = 0,
    nodeType = Output,
    inputs = Array.fromList [],
    outputs = Array.fromList []
 }

type NodeType = Input String TensorType
              | Output
              | Constant Tensor
              | Variable
              | Zeros TensorType
              | Add
              | Sub
              | Mul
              | Div
              | Mod
              | Neg
              | Log
              | Equal
              | Argmax Int
              | Cast TensorType
              | RandomNormal Float Float
              | MatMul
              | SoftMax
              | ReduceMean
              | ReduceSum (List Int)
              | TrainGDOMinimize Float

nbInputs : NodeType -> Int
nbInputs nodeType = Array.length (inputTypes nodeType)

-- Input types of operations, defines the size of the input list of the node as well
inputTypes : NodeType -> Array TensorType
inputTypes nodeType = 
    case nodeType of
        Input _ _ -> Array.fromList []
        Output -> Array.fromList [AnyTensor]
        Constant _ -> Array.fromList []
        Variable -> Array.fromList [AnyTensor]
        Zeros _ -> Array.fromList[IntTensor]
        Add -> Array.fromList [NumberTensor, NumberTensor]
        Sub -> Array.fromList [NumberTensor, NumberTensor]
        Mul -> Array.fromList [NumberTensor, NumberTensor]
        Div -> Array.fromList [NumberTensor, NumberTensor]
        Mod -> Array.fromList [NumberTensor, NumberTensor]
        Neg -> Array.fromList [NumberTensor]
        Log -> Array.fromList [NumberTensor]
        Equal -> Array.fromList [AnyTensor, AnyTensor]
        Argmax _ -> Array.fromList [NumberTensor]
        Cast _ -> Array.fromList [AnyTensor]
        RandomNormal _ _ -> Array.fromList [IntTensor]
        MatMul -> Array.fromList [NumberTensor, NumberTensor]
        SoftMax -> Array.fromList [NumberTensor]
        ReduceMean -> Array.fromList [NumberTensor]
        ReduceSum _ -> Array.fromList [NumberTensor]
        TrainGDOMinimize _ -> Array.fromList[NumberTensor]

nbOutputs : NodeType -> Int
nbOutputs nodeType = Array.length (outputTypes nodeType)

-- Output types of operations, defines the size of the output list of the node as well
outputTypes : NodeType -> Array TensorType
outputTypes nodeType =
    case nodeType of
        Input _ _ -> Array.fromList [AnyTensor]
        Output -> Array.fromList []
        Constant _ -> Array.fromList [AnyTensor]
        Variable -> Array.fromList [AnyTensor]
        Zeros _ -> Array.fromList [FloatTensor]
        Add -> Array.fromList [NumberTensor]
        Sub -> Array.fromList [NumberTensor]
        Mul -> Array.fromList [NumberTensor]
        Div -> Array.fromList [NumberTensor]
        Mod -> Array.fromList [NumberTensor]
        Neg -> Array.fromList [NumberTensor]
        Log -> Array.fromList [NumberTensor]
        Equal -> Array.fromList [BoolTensor]
        Argmax _ -> Array.fromList [IntTensor]
        Cast t -> Array.fromList [t]
        RandomNormal _ _ -> Array.fromList [FloatTensor]
        MatMul -> Array.fromList [NumberTensor]
        SoftMax -> Array.fromList [NumberTensor]
        ReduceMean -> Array.fromList [NumberTensor]
        ReduceSum _ -> Array.fromList [NumberTensor]
        TrainGDOMinimize _ -> Array.fromList[AnyTensor]

-- Checks if type1 can be passed as an argument of type2
compatibleTypes : TensorType -> TensorType -> Bool
compatibleTypes type1 type2 = case type1 of
    NumberTensor -> type2 == NumberTensor || type2 == IntTensor || type2 == FloatTensor || type2 == AnyTensor
    IntTensor -> type2 == IntTensor || type2 == NumberTensor || type2 == AnyTensor
    FloatTensor -> type2 == FloatTensor || type2 == NumberTensor || type2 == AnyTensor
    AnyTensor -> True
    NoTensor -> False
    _ -> type2 == type1 || type2 == AnyTensor

-- Create an edge between two nodes on designated in/outgoing points
bindNodes : (Int, Int) -> (Int, Int) -> Tree -> Maybe Tree
bindNodes (nodeId1, id1) (nodeId2, id2) tree = let
        node1 = case List.head (List.filter (\x -> x.id == nodeId1) tree.nodes) of
            Just n1 -> case (Array.get id1 n1.outputs) of
                Just Nothing -> n1
                _ -> dummyNode
            Nothing -> dummyNode
        node1OutType = case Array.get id1 (outputTypes node1.nodeType) of
            Just type1 -> type1
            Nothing -> NoTensor
        node2 = case List.head (List.filter (\x -> x.id == nodeId2) tree.nodes) of
            Just n2 -> case (Array.get id2 n2.inputs) of
                Just Nothing -> n2
                _ -> dummyNode
            Nothing -> dummyNode
        node2InType = case Array.get id2 (inputTypes node2.nodeType) of
            Just type2 -> type2
            Nothing -> NoTensor
        newNode1 = { node1 | outputs = Array.set id1 (Just node2.id) node1.outputs }
        newNode2 = { node2 | inputs = Array.set id2 (Just node1.id) node2.inputs }
        otherNodes = List.filter (\x -> x.id /= node1.id && x.id /= node2.id) tree.nodes
        newNodes = List.append otherNodes [newNode1, newNode2]
    in if (compatibleTypes node1OutType node2InType)
        then Just { tree | nodes = newNodes }
        else Nothing

unbindNodes : (Node, Int) -> (Node, Int) -> Tree -> Tree
unbindNodes (node1, id1) (node2, id2) tree = let
        newNode1 = { node1 | outputs = Array.set id1 (Nothing) node1.outputs }
        newNode2 = { node2 | inputs = Array.set id2 (Nothing) node2.inputs }
        otherNodes = List.filter (\x -> x.id /= node1.id && x.id /= node2.id) tree.nodes
        newNodes = List.append otherNodes [newNode1, newNode2]
    in { tree | nodes = newNodes }

newTree : Tree
newTree = { nodes = [] }

-- Adds a node with the given ID (have to generate all different IDs elsewhere). Returns the modified tree
-- Attention : IDs have to start with 1 ! (0 is error value)
addNode : NodeType -> Int -> Tree -> Tree
addNode nodeType nodeId tree = { tree | nodes = List.append tree.nodes [{
        id = nodeId,
        nodeType = nodeType,
        inputs = Array.repeat (nbInputs nodeType) Nothing,
        outputs = Array.repeat (nbOutputs nodeType) Nothing
    }] }

deleteNode : Node -> Tree -> Tree
deleteNode node tree = { tree | nodes = List.filter (\x -> x /= node) tree.nodes }
