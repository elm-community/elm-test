module Main where

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)
import IO.Runner as Run

import Test
import ElmTest.Runner.Console as Console

sigs : IO ()
sigs = Console.runDisplay Test.suite3

-- | Can't use a type alias in ports, yet :/
port requests : Signal Request
port requests = Run.run responses sigs

port responses : Signal Response
