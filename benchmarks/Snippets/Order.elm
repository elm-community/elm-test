module Snippets.Order exposing (test1, test2)

import Expect exposing (Expectation)
import Fuzz
import Test exposing (Test, fuzz)


test1 : Test
test1 =
    fuzz (Fuzz.list Fuzz.int) "(passes) reversing an ordering function" <|
        \numbers ->
            let
                ordered =
                    List.sortWith compare numbers

                reverseOrdered =
                    List.sortWith (reverseOrder compare) numbers
            in
            reverseOrdered
                |> List.reverse
                |> Expect.equal ordered


test2 : Test
test2 =
    fuzz (Fuzz.list Fuzz.int) "(fails) reversing an ordering function" <|
        \numbers ->
            let
                ordered =
                    List.sortWith compare numbers

                reverseOrdered =
                    List.sortWith (reverseOrder compare) numbers
            in
            reverseOrdered
                |> Expect.equal ordered


type alias CompareFunction a =
    a -> a -> Order


reverseOrder : CompareFunction a -> CompareFunction a
reverseOrder compareFn a b =
    case compareFn a b of
        EQ ->
            EQ

        GT ->
            LT

        LT ->
            GT
