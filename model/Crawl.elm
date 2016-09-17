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
    in prev ++ actualCode node.nodeType (unwrap node.inputs) (unwrap node.outputs)

findNode : Int -> Tree -> Maybe Node
findNode id tree = List.head (List.filter (\node -> node.id == id) tree.nodes)

unwrap : Array.Array (Maybe Int) -> Array.Array Int
unwrap array = Array.map (\x -> case x of
        Just val -> val
        Nothing -> 0
    ) array

actualCode : NodeType -> Array.Array Int -> Array.Array Int -> String
actualCode nodeType inputs outputs = "\n"