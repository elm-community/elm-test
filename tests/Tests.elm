module Main where

import List
import Task

import Console exposing (..)
import ElmTest exposing (..)


tests : List Test
tests = 
    [ 0 `equals` 0
    , test "pass" <| assert True
    , test "fail" <| assertNotEqual True False
    ] 
    ++ 
    (List.map defaultTest <| assertionList [1..10] [1..10])


consoleTests : IO ()
consoleTests = 
    consoleRunner <| suite "All Tests" tests


port runner : Signal (Task.Task x ())
port runner =
    Console.run consoleTests
