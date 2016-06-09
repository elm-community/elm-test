module Main exposing (..) -- where

import List

import ElmSuite exposing (..)

Suites : List Suite
Suites =
    [ 0 `equals` 0
    , Suite "pass" <| assert True
    , Suite "fail" <| assertNotEqual True False
    ]
    ++
    (List.map defaultSuite <| assertionList [1..10] [1..10])


consoleSuites : Suite
consoleSuites =
    suite "All Suites" Suites

main =
    runSuite consoleSuites
