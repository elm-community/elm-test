module Test.Runner.Tests exposing (all)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import Test.Runner exposing (SeededRunners(..))
import Random.Pcg as Random


all : Test
all =
    Test.concat
        [ fromTest ]


toSeededRunners : Test -> SeededRunners
toSeededRunners =
    Test.Runner.fromTest 5 (Random.initialSeed 42)


fromTest : Test
fromTest =
    describe "TestRunner.fromTest"
        [ describe "test length"
            [ fuzz2 int int "only positive tests runs are valid" <|
                \runs intSeed ->
                    case Test.Runner.fromTest runs (Random.initialSeed intSeed) passing of
                        Invalid str ->
                            if runs > 0 then
                                Expect.fail ("Expected a run count of " ++ toString runs ++ " to be valid, but was invalid with this message: " ++ toString str)
                            else
                                Expect.pass

                        val ->
                            if runs > 0 then
                                Expect.pass
                            else
                                Expect.fail ("Expected a run count of " ++ toString runs ++ " to be invalid, but was valid with this value: " ++ toString val)
            , test "a test that uses only is an Only summary" <|
                \() ->
                    case toSeededRunners (Test.only <| test "passes" (\() -> Expect.pass)) of
                        Only runners ->
                            runners
                                |> List.length
                                |> Expect.equal 1

                        val ->
                            Expect.fail ("Expected SeededRunner to be Only, but was " ++ toString val)
            , test "a pair of tests where one uses skip is a Skipping summary" <|
                \() ->
                    let
                        runners =
                            toSeededRunners <|
                                describe "two tests"
                                    [ test "passes" <|
                                        \() -> Expect.pass
                                    , Test.skip <|
                                        test "fails" <|
                                            \() -> Expect.fail "failed on purpose"
                                    ]
                    in
                        case runners of
                            Skipping runners ->
                                runners
                                    |> List.length
                                    |> Expect.equal 1

                            val ->
                                Expect.fail ("Expected SeededRunner to be Skipping, but was " ++ toString val)
            , test "a test that uses skip is an empty Skipping summary" <|
                \() ->
                    case toSeededRunners (Test.skip <| test "passes" (\() -> Expect.pass)) of
                        Skipping runners ->
                            runners
                                |> List.length
                                |> Expect.equal 0

                        val ->
                            Expect.fail ("Expected SeededRunner to be Skipping, but was " ++ toString val)
            , test "a test that does not use only or skip is a Plain summary" <|
                \() ->
                    case toSeededRunners (test "passes" (\() -> Expect.pass)) of
                        Plain runners ->
                            runners
                                |> List.length
                                |> Expect.equal 1

                        val ->
                            Expect.fail ("Expected SeededRunner to be Plain, but was " ++ toString val)
            ]
        ]


passing : Test
passing =
    test "A passing test" (\() -> Expect.pass)
