module Main where

import Task

import Console exposing (..)
import ElmTest exposing (consoleRunner)

import Test


tests : IO ()
tests = 
    consoleRunner Test.suite3


port runner : Signal (Task.Task x ())
port runner = 
    Console.run tests
