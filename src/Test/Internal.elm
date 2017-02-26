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
duplicatedName tests =
    let
        name : Test -> Maybe String
        name test =
            case test of
                Labeled str _ ->
                    Just str

                _ ->
                    Nothing

        helper : Set String -> List Test -> Result String (Set String)
        helper set tests =
            case tests of
                [] ->
                    Ok set

                t :: ts ->
                    case name t of
                        Nothing ->
                            helper set ts

                        Just aName ->
                            if Set.member aName set then
                                Err aName
                            else
                                helper (Set.insert aName set) ts
    in
        helper Set.empty tests
