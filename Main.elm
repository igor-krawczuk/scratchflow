import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

import Html.Events exposing (on)

import Json.Decode as Json exposing ((:=))
import Window as Window
import Task
import Mouse exposing (Position)
import Dict
--MY IMPORTS
import Selector
import GraphicalNode
import GraphArea
import GUI exposing (..)


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- INIT
init = helperGetInit
