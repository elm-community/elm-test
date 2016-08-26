module Tests exposing (all)

import Test exposing (..)
import Fuzz exposing (..)
import Set
import String
import Expect


all : Test
all =
    Test.concat
        [ readmeExample, bug39 ]


{-| Regression test for https://github.com/elm-community/elm-test/issues/39
-}
bug39 : Test
bug39 =
    fuzz (intRange 1 32) "small slice end" <|
        \positiveInt ->
            positiveInt
                |> Expect.greaterThan 0


readmeExample : Test
readmeExample =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                \() ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)
            , test "reverses a known string" <|
                \() ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            , test "equal sets" <|
                \() ->
                    (Set.fromList [ 1, 2, 3 ])
                        |> Expect.equalSets (Set.fromList [ 1, 2, 3 ])
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
