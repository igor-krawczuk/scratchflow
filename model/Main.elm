import Html
import Html.Attributes
import Array
import Maybe exposing (andThen)

import Tree exposing (..)
import Crawl exposing (..)

main = let
    {- Full tree structure
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

    -- Step 1 : create the nodes
    basetree = newTree
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
    tree1 = basetree
     |> addNode (Log) 14
     |> addNode (Mul) 15
     |> addNode (ReduceSum [1]) 16
     |> addNode (Neg) 17
     |> addNode (ReduceMean) 18
     |> addNode (TrainGDOMinimize 0.5) 19
     |> addNode (Output) 20
    tree2 = basetree
     |> addNode (Argmax 1) 21
     |> addNode (Argmax 1) 22
     |> addNode (Equal) 23
     |> addNode (Cast FloatTensor) 24
     |> addNode (ReduceMean) 25
     |> addNode (Output) 26

    -- Step 2 : Link the nodes
    baseGraph tr = Just tr
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
    graph1 = baseGraph tree1
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
    graph2 = baseGraph tree2
        -- correct_prediction = tf.equal(tf.argmax(y, 1), tf.argmax(y_, 1))
        `andThen` (\t -> bindNodes (11,0) (21,0) t)
        `andThen` (\t -> bindNodes (13,0) (22,0) t)
        `andThen` (\t -> bindNodes (21,0) (23,0) t)
        `andThen` (\t -> bindNodes (22,0) (23,1) t)
        -- accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))
        `andThen` (\t -> bindNodes (23,0) (24,0) t)
        `andThen` (\t -> bindNodes (24,0) (25,0) t)
        `andThen` (\t -> bindNodes (25,0) (26,0) t)

    -- Step 3 : Crawl the graph to get the code
    code1 = case graph1 of
        Just c -> crawl c
        Nothing -> ""
    code2 = case graph2 of
        Just c -> crawl c
        Nothing -> ""
    in Html.div [] [
        Html.textarea [Html.Attributes.cols 80, Html.Attributes.rows 25] [Html.text code1],
        Html.textarea [Html.Attributes.cols 80, Html.Attributes.rows 25] [Html.text code2]
    ]
