module Test.Runner.String exposing (run, runWithOptions)

{-| # String Runner

@docs run, runWithOptions
-}

import Random.Pcg as Random
import Test exposing (Test)
import Assert exposing (Assertion)
import String
import Test.Runner exposing (Runner(..))


toOutput : ( String, Int ) -> Runner -> ( String, Int )
toOutput =
    flip (toOutputHelp [])


toOutputHelp : List String -> Runner -> ( String, Int ) -> ( String, Int )
toOutputHelp labels runner tuple =
    case runner of
        Runnable runnable ->
            List.foldl (fromAssertion labels) tuple (Test.Runner.run runnable)

        Labeled label subRunner ->
            toOutputHelp (label :: labels) subRunner tuple

        Batch runners ->
            List.foldl (toOutputHelp labels) tuple runners


fromAssertion : List String -> Assertion -> ( String, Int ) -> ( String, Int )
fromAssertion labels assertion tuple =
    case Assert.getFailure assertion of
        Nothing ->
            tuple

        Just message ->
            let
                ( output, failureCount ) =
                    tuple
            in
                ( String.join "\n\n" [ output, (withoutEmptyStrings >> outputFailures message) labels ]
                , failureCount + 1
                )


withoutEmptyStrings : List String -> List String
withoutEmptyStrings =
    List.filter ((/=) "")


outputFailures : String -> List String -> String
outputFailures message labels =
    let
        ( maybeLastLabel, otherLabels ) =
            case labels of
                [] ->
                    ( Nothing, [] )

                first :: rest ->
                    ( Just first, List.reverse rest )

        outputMessage message =
            case maybeLastLabel of
                Just label ->
                    String.join "\n\n"
                        [ "✗ " ++ label, message ]

                Nothing ->
                    message

        outputContext =
            otherLabels
                |> List.map ((++) "↓ ")
                |> String.join "\n"
    in
        outputContext ++ "\n" ++ outputMessage message


defaultSeed : Random.Seed
defaultSeed =
    Random.initialSeed 42


defaultRuns : Int
defaultRuns =
    100


{-| TODO document
-}
run : Test -> ( String, Int )
run =
    runWithOptions defaultRuns defaultSeed


{-| TODO document
-}
runWithOptions : Int -> Random.Seed -> Test -> ( String, Int )
runWithOptions runs seed test =
    test
        |> Test.Runner.fromTest runs seed
        |> toOutput ( "", 0 )
