import Html
import Html.Attributes
import Array

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
    code = newTree
     |> addNode (Constant (Scalar 1)) 1
     |> addNode (Constant (Scalar 2)) 2
     |> addNode AddType 3
     |> addNode Output 4
     |> bindNodes (1,0) (3,0)
     |> bindNodes (2,0) (3,1)
     |> bindNodes (3,0) (4,0)
     --|> toString
     |> crawl
    in Html.textarea [Html.Attributes.cols 80, Html.Attributes.rows 25] [Html.text code]