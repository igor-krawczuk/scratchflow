import Html
import Html.Attributes
import Array
import Maybe exposing (andThen)

import Tree exposing (..)
import Crawl exposing (..)

main = let
    {-
    tree = {
        nodes = [
            {
                id = 1,
                nodeType = Constant (Scalar 1),
                inputs = Array.empty,
                outputs = Array.fromList [Just 3]
            },
            {
                id = 2,
                nodeType = Constant (Scalar 2),
                inputs = Array.empty,
                outputs = Array.fromList [Just 3]
            },
            {
                id = 3,
                nodeType = AddType,
                inputs = Array.fromList [Just 1, Just 2],
                outputs = Array.fromList [Just 4]
            },
            {
                id = 4,
                nodeType = Output,
                inputs = Array.fromList [Just 3],
                outputs = Array.empty
            }
        ]
    }
    -}
    tree = newTree
     |> addNode (Constant (Vector [1,2])) 1
     |> addNode (Constant (Vector [2,3])) 2
     |> addNode Add 3
     |> addNode Output 4
     |> addNode (RandomNormal 1 0.5) 5
     --|> toString
    graph = Just tree
        `andThen` (\t -> bindNodes (1,0) (3,0) t)
        `andThen` (\t -> bindNodes (2,0) (3,1) t)
        `andThen` (\t -> bindNodes (3,0) (5,0) t)
        `andThen` (\t -> bindNodes (5,0) (4,0) t)

    code = case graph of
        Just c -> crawl c
        Nothing -> ""
     --|> toString
    in Html.textarea [Html.Attributes.cols 80, Html.Attributes.rows 25] [Html.text code]