module Helpers exposing (different, expectPass, expectToFail, randomSeedFuzzer, same, succeeded, testShrinking, testStringLengthIsPreserved)

import Expect
import Fuzz exposing (Fuzzer)
import Random.Pcg as Random
import Shrink
import Test exposing (Test)
import Test.Expectation exposing (Expectation(..))
import Test.Internal as Internal
import Test.Runner.Failure exposing (Reason(..))


expectPass : a -> Expectation
expectPass _ =
    Expect.pass


testStringLengthIsPreserved : List String -> Expectation
testStringLengthIsPreserved strings =
    strings
        |> List.map String.length
        |> List.sum
        |> Expect.equal (String.length (List.foldl (++) "" strings))


expectToFail : Test -> Test
expectToFail =
    expectFailureHelper (always Nothing)


succeeded : Expectation -> Bool
succeeded expectation =
    case expectation of
        Pass ->
            True

        Fail _ ->
            False


passesToFails :
    ({ reason : Reason
     , description : String
     , given : Maybe String
     }
     -> Maybe String
    )
    -> List Expectation
    -> List Expectation
passesToFails f expectations =
    expectations
        |> List.filterMap (passToFail f)
        |> List.map Expect.fail
        |> (\list ->
                if List.isEmpty list then
                    [ Expect.pass ]
                else
                    list
           )


passToFail :
    ({ reason : Reason
     , description : String
     , given : Maybe String
     }
     -> Maybe String
    )
    -> Expectation
    -> Maybe String
passToFail f expectation =
    case expectation of
        Pass ->
            Just "Expected this test to fail, but it passed!"

        Fail record ->
            f record


expectFailureHelper : ({ description : String, given : Maybe String, reason : Reason } -> Maybe String) -> Test -> Test
expectFailureHelper f test =
    case test of
        Internal.UnitTest runTest ->
            Internal.UnitTest <|
                \() ->
                    passesToFails f (runTest ())

        Internal.FuzzTest runTest ->
            Internal.FuzzTest <|
                \seed runs ->
                    passesToFails f (runTest seed runs)

        Internal.Labeled desc labeledTest ->
            Internal.Labeled desc (expectFailureHelper f labeledTest)

        Internal.Batch tests ->
            Internal.Batch (List.map (expectFailureHelper f) tests)

        Internal.Skipped subTest ->
            expectFailureHelper f subTest
                |> Internal.Skipped

        Internal.Only subTest ->
            expectFailureHelper f subTest
                |> Internal.Only


testShrinking : Test -> Test
testShrinking =
    let
        handleFailure { given, description } =
            let
                acceptable =
                    String.split "|" description
            in
            case given of
                Nothing ->
                    Just "Expected this test to have a given value!"

                Just g ->
                    if List.member g acceptable then
                        Nothing
                    else
                        Just <| "Got shrunken value " ++ g ++ " but expected " ++ String.join " or " acceptable
    in
    expectFailureHelper handleFailure


{-| get a good distribution of random seeds, and don't shrink our seeds!
-}
randomSeedFuzzer : Fuzzer Random.Seed
randomSeedFuzzer =
    Fuzz.custom (Random.int 0 0xFFFFFFFF) Shrink.noShrink |> Fuzz.map Random.initialSeed


same : Expectation -> Expectation -> Expectation
same a b =
    case ( a, b ) of
        ( Test.Expectation.Pass, Test.Expectation.Pass ) ->
            Test.Expectation.Pass

        ( Test.Expectation.Fail _, Test.Expectation.Fail _ ) ->
            Test.Expectation.Pass

        ( a, b ) ->
            Test.Expectation.fail { description = "expected both arguments to fail, or both to succeed", reason = Equality (toString a) (toString b) }


different : Expectation -> Expectation -> Expectation
different a b =
    case ( a, b ) of
        ( Test.Expectation.Pass, Test.Expectation.Fail _ ) ->
            Test.Expectation.Pass

        ( Test.Expectation.Fail _, Test.Expectation.Pass ) ->
            Test.Expectation.Pass

        ( a, b ) ->
            Test.Expectation.fail { description = "expected one argument to fail", reason = Equality (toString a) (toString b) }
