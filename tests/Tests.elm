module Main exposing (..) -- where

import Array
import List
import Json.Decode

import Html.App as Html
import Html
import ElmTest exposing (..)

tests : List Test
tests =
    [ 0 `equals` 0
    , test "pass" <| assert False
    , test "fail" <| assertNotEqual True False
    ]
    ++
    (List.map defaultTest <| assertionList [1..10] [1..10])


consoleTests : String
consoleTests =
    consoleRunner <| suite "All Tests" tests

main =
    Html.beginnerProgram
        { model = ""
        , view = (\x -> Html.text consoleTests)
        , update = (\x y -> y)
        }
