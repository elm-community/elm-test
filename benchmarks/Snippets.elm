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


intRangePass : Test
intRangePass =
    fuzz (Fuzz.intRange 10 100) "(passes) intRange" <|
        \_ ->
            Expect.pass


intRangeFail : Test
intRangeFail =
    fuzz (Fuzz.intRange 10 100) "(fails) intRange" <|
        \numbers ->
            Expect.fail "Failed"


stringPass : Test
stringPass =
    fuzz Fuzz.string "(passes) string" <|
        \_ ->
            Expect.pass


stringFail : Test
stringFail =
    fuzz Fuzz.string "(fails) string" <|
        \numbers ->
            Expect.fail "Failed"


floatPass : Test
floatPass =
    fuzz Fuzz.float "(passes) float" <|
        \_ ->
            Expect.pass


floatFail : Test
floatFail =
    fuzz Fuzz.float "(fails) float" <|
        \numbers ->
            Expect.fail "Failed"


boolPass : Test
boolPass =
    fuzz Fuzz.bool "(passes) bool" <|
        \_ ->
            Expect.pass


boolFail : Test
boolFail =
    fuzz Fuzz.bool "(fails) bool" <|
        \numbers ->
            Expect.fail "Failed"


charPass : Test
charPass =
    fuzz Fuzz.char "(passes) char" <|
        \_ ->
            Expect.pass


charFail : Test
charFail =
    fuzz Fuzz.char "(fails) char" <|
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


maybeIntPass : Test
maybeIntPass =
    fuzz (Fuzz.maybe Fuzz.int) "(passes) maybe of int" <|
        \_ ->
            Expect.pass


maybeIntFail : Test
maybeIntFail =
    fuzz (Fuzz.maybe Fuzz.int) "(fails) maybe of int" <|
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
