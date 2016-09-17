import List
import Array
import String
import Maybe

import Tree exposing (..)

crawl : Tree -> String
crawl tree = let
    outputNodes = List.filter (\node -> case node.nodeType of 
        Output -> True
        _ -> False
    ) tree.nodes
    in String.concat (List.map (\node ->
            generateCode node tree
        ) outputNodes)

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

unwrap : Array.Array (Maybe Int) -> Array.Array Int
unwrap array = Array.map (\x -> case x of
        Just val -> val
        Nothing -> 0
    ) array

actualCode : NodeType -> Int -> Array.Array Int -> Array.Array Int -> String
actualCode nodeType nodeId inputs outputs = case nodeType of
    Input x -> "\n"
    Constant x -> "v" ++ (toString nodeId) ++ " = tf.constant(" ++ (toString x) ++ ")\n"
    Variable x -> "v" ++ (toString nodeId) ++ " = tf.Variable(" ++ (toString x) ++ ")\n"
    AddType -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.add(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    SubType -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.sub(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    MulType -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.mul(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    DivType -> let
        id1 = Array.get 0 inputs
        id2 = Array.get 1 inputs
        in case id1 of
            Just ident1 -> case id2 of
                Just ident2 -> "v" ++ (toString nodeId) ++ " = tf.div(v" ++ (toString ident1) ++ ", v" ++ (toString ident2) ++ ")\n"
                _ -> ""
            _ -> ""
    _ -> "\n"