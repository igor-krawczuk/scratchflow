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
     |> addNode (Constant (Vector [0, 784])) 1
     |> addNode (Input "x" FloatTensor) 2
     |> addNode (Constant (Vector [784, 10])) 3
     |> addNode (Zeros FloatTensor) 4
     |> addNode (Variable) 5
     |> addNode (Constant (Vector [10])) 6
     |> addNode (Zeros FloatTensor) 7
     |> addNode (Variable) 8
     |> addNode (MatMul) 9
     |> addNode (Add) 10
     |> addNode (SoftMax) 11
     |> addNode (Constant (Vector [0, 10])) 12
     |> addNode (Input "y_" FloatTensor) 13
     |> addNode (Log) 14
     |> addNode (Mul) 15
     |> addNode (ReduceSum [1]) 16
     |> addNode (Neg) 17
     |> addNode (ReduceMean) 18
     |> addNode (TrainGDOMinimize 0.5) 19
     |> addNode (Output) 20

     --|> toString
    graph = Just tree
        -- x = tf.placeholder(tf.float32, [None, 784])
        `andThen` (\t -> bindNodes (1,0) (2,0) t)        
        -- W = tf.Variable(tf.zeros([784, 10]))
        `andThen` (\t -> bindNodes (3,0) (4,0) t)
        `andThen` (\t -> bindNodes (4,0) (5,0) t)
        -- b = tf.Variable(tf.zeros([10]))
        `andThen` (\t -> bindNodes (6,0) (7,0) t)
        `andThen` (\t -> bindNodes (7,0) (8,0) t)
        -- y = tf.nn.softmax(tf.matmul(x, W) + b)
        `andThen` (\t -> bindNodes (2,0) (9,0) t)
        `andThen` (\t -> bindNodes (5,0) (9,1) t)
        `andThen` (\t -> bindNodes (9,0) (10,0) t)
        `andThen` (\t -> bindNodes (8,0) (10,1) t)
        `andThen` (\t -> bindNodes (10,0) (11,0) t)
        -- y_ = tf.placeholder(tf.float32, [None, 10])
        `andThen` (\t -> bindNodes (12,0) (13,0) t)                
        -- cross_entropy = tf.reduce_mean(-tf.reduce_sum(y_ * tf.log(y), reduction_indices=[1]))
        `andThen` (\t -> bindNodes (11,0) (14,0) t)
        `andThen` (\t -> bindNodes (14,0) (15,0) t)
        `andThen` (\t -> bindNodes (13,0) (15,1) t)
        `andThen` (\t -> bindNodes (15,0) (16,0) t)
        `andThen` (\t -> bindNodes (16,0) (17,0) t)
        `andThen` (\t -> bindNodes (17,0) (18,0) t)
        -- train_step = tf.train.GradientDescentOptimizer(0.5).minimize(cross_entropy)
        `andThen` (\t -> bindNodes (18,0) (19,0) t)
        `andThen` (\t -> bindNodes (19,0) (20,0) t)

    code = case graph of
        Just c -> crawl c
        Nothing -> ""
     --|> toString
    in Html.textarea [Html.Attributes.cols 80, Html.Attributes.rows 25] [Html.text code]
    --in Html.textarea [Html.Attributes.cols 80, Html.Attributes.rows 25] [Html.text (toString graph)]