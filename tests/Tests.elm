module Main exposing (..) -- where

import List

import ElmTest exposing (..)

tests : List Test
tests =
    [ 0 `equals` 0
    , test "pass" <| assert True
    , test "fail" <| assertNotEqual True False
    ]
    ++
    (List.map defaultTest <| assertionList [1..10] [1..10])


consoleTests : Test
consoleTests =
    suite "All Tests" tests

main =
    runSuite consoleTests
