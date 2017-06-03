module Snippets exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, fuzz)


intPass : Test
intPass =
    fuzz Fuzz.int "(passes) int" <|
        \_ ->
            Expect.pass


intFail : Test
intFail =
    fuzz Fuzz.int "(fails) int" <|
        \numbers ->
            Expect.fail "Failed"


listIntPass : Test
listIntPass =
    fuzz (Fuzz.list Fuzz.int) "(passes) list of int" <|
        \_ ->
            Expect.pass


listIntFail : Test
listIntFail =
    fuzz (Fuzz.list Fuzz.int) "(fails) list of int" <|
        \numbers ->
            Expect.fail "Failed"


andMapPass : Test
andMapPass =
    fuzz range "(passes) andMap" <|
        \_ -> Expect.pass


andMapFail : Test
andMapFail =
    fuzz range "(fails) andMap" <|
        \_ -> Expect.fail "Failed"


type alias Person =
    { firstName : String
    , lastName : String
    , age : Int
    , nationality : String
    , height : Float
    }


range : Fuzzer Person
range =
    Fuzz.map Person Fuzz.string
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.float
