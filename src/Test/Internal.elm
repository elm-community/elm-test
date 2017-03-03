module Test.Internal exposing (Test(..), failNow, filter, duplicatedName)

import Random.Pcg as Random exposing (Generator)
import Test.Expectation exposing (Expectation(..))
import Set exposing (Set)


type Test
    = Test (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Batch (List Test)


{-| Create a test that always fails for the given reason and description.
-}
failNow : { description : String, reason : Test.Expectation.Reason } -> Test
failNow record =
    Test
        (\_ _ -> [ Test.Expectation.fail record ])


filter : (String -> Bool) -> Test -> Test
filter =
    filterHelp False


filterHelp : Bool -> (String -> Bool) -> Test -> Test
filterHelp lastCheckPassed isKeepable test =
    case test of
        Test _ ->
            if lastCheckPassed then
                test
            else
                Batch []

        Labeled desc labeledTest ->
            labeledTest
                |> filterHelp (isKeepable desc) isKeepable
                |> Labeled desc

        Batch tests ->
            tests
                |> List.map (filterHelp lastCheckPassed isKeepable)
                |> Batch


duplicatedName : List Test -> Result String (Set String)
duplicatedName =
    let
        names : Test -> List String
        names test =
            case test of
                Labeled str _ ->
                    [ str ]

                Batch subtests ->
                    List.concatMap names subtests

                Test _ ->
                    []

        insertOrFail : String -> Result String (Set String) -> Result String (Set String)
        insertOrFail newName =
            Result.andThen
                (\oldNames ->
                    if Set.member newName oldNames then
                        Err newName
                    else
                        Ok <| Set.insert newName oldNames
                )
    in
        List.concatMap names
            >> List.foldl insertOrFail (Ok Set.empty)
