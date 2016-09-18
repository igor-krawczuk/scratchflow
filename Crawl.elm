module Crawl exposing (crawl)

import List
import Array
import String
import Maybe

import Tree exposing (..)

-- Main tree transformation function (code is concatenated if multiple outputs are here)
-- (Note : Nodes with multiple children will be put twice in the code)
-- A cycle in the graph will create infinite loops !!!
crawl : Tree -> String
crawl tree = let
    outputNodes = List.filter (\node -> case node.nodeType of 
        Output -> True
        _ -> False
    ) tree.nodes
    in String.concat (List.map (\node ->
            generateCode node tree
        ) outputNodes)

-- Recursive function going through the tree from the output
generateCode : Node -> Tree -> String
generateCode node tree = let
    prev = Array.foldl (++) "" (Array.map (\nodeId ->
            case nodeId of
                Just id -> case findNode id tree of
                    Just prevNode -> generateCode prevNode tree
                    Nothing -> ""
                Nothing -> ""
        ) node.inputs)
    in prev ++ actualCode node.nodeType node.id (unwrap node.inputs) (unwrap node.outputs)

findNode : Int -> Tree -> Maybe Node
findNode id tree = List.head (List.filter (\node -> node.id == id) tree.nodes)

-- Index 0 means error (node indexes begin at 1)
unwrap : Array.Array (Maybe Int) -> Array.Array Int
unwrap array = Array.map (\x -> case x of
        Just val -> val
        Nothing -> 0
    ) array

-- Returns the line of python code equivalent to the operation
actualCode : NodeType -> Int -> Array.Array Int -> Array.Array Int -> String
actualCode nodeType nodeId inputs outputs = case nodeType of
    Input name t -> name ++ " = tf.placeholder(" ++ (displayTensorType t) ++ ")\n"
    Constant x -> "v" ++ (toString nodeId) ++ " = tf.constant(" ++ (displayTensor x) ++ ")\n"
    Variable -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.Variable(v" ++ (toString ident) ++ ")\n"
            _ -> ""
    Zeros t -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.zeros(v" ++ (toString ident) ++ ", " ++ (displayTensorType t) ++ ")\n"
            _ -> ""
    Add -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.add(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    Sub -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.sub(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    Mul -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.mul(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    Div -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.div(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    Mod -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.mod(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    Neg -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.neg(v" ++ (toString ident) ++  ")\n"
            _ -> ""
    Log -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.log(v" ++ (toString ident) ++  ")\n"
            _ -> ""
    Equal -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.equal(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    Argmax d -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.argmax(v" ++ (toString ident) ++ ", " ++ (toString d) ++ ")\n"
            _ -> ""
    Cast t -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.cast(v" ++ (toString ident) ++ ", " ++ (displayTensorType t) ++ ")\n"
            _ -> ""
    RandomNormal mean stddev -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.random_normal(v" ++ (toString ident) ++ ", " ++ (toString mean) ++ ", " ++ (toString stddev) ++ ")\n"
            _ -> ""
    MatMul -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.matmul(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    SoftMax -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.nn.softmax(v" ++ (toString ident) ++ ")\n"
            _ -> ""
    ReduceMean -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.reduce_mean(v" ++ (toString ident) ++ ")\n"
            _ -> ""
    ReduceSum d -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.reduce_sum(v" ++ (toString ident) ++ ", " ++ (toString d) ++ ")\n"
            _ -> ""
    TrainGDOMinimize f -> let
        id = Array.get 0 inputs
        in case id of
            Just ident -> "v" ++ (toString nodeId) ++ " = tf.train.GradientDescentOptimizer(" ++ (toString f) ++ ").minimize(v" ++ (toString ident) ++ ")\n"
            _ -> ""
    _ -> "\n"

displayTensor : Tensor -> String
displayTensor tensor = case tensor of
    Scalar s -> toString s
    Vector s -> toString s
    Matrix s -> toString s
    Cube s -> toString s

displayTensorType : TensorType -> String
displayTensorType tensorType = case tensorType of
    NumberTensor -> "tf.float32"
    IntTensor -> "tf.int32"
    FloatTensor -> "tf.float32"
    BoolTensor -> "tf.bool"
    StringTensor -> "tf.string"
    AnyTensor -> ""
    NoTensor -> ""