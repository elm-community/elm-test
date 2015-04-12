module Main where

import List

import ElmTest.Assertion as A
import ElmTest.Run as R
import ElmTest.Runner.Console exposing (runDisplay)
import ElmTest.Test exposing (..)

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)
import IO.Runner as Run

tests : List Test
tests = [ (R.run (0 `equals` 0)) `equals` (R.Pass "0 == 0")
        , test "pass" <| A.assert (R.pass <| R.Pass "")
        , test "fail" <| A.assertNotEqual (R.fail <| R.Pass "") True
        ] ++ (List.map defaultTest <| A.assertionList [1..10] [1..10])

console = runDisplay <| Suite "All Tests" tests

port requests : Signal Request
port requests = Run.run responses console

port responses : Signal Response
