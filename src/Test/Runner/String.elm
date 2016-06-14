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
                ( String.join "\n\n" [ output, outputFailures labels message ]
                , failureCount + 1
                )


outputFailures : List String -> String -> String
outputFailures labels message =
    let
        ( maybeLastContext, otherContexts ) =
            case labels of
                [] ->
                    ( Nothing, [] )

                first :: rest ->
                    ( Just first, List.reverse rest )

        outputMessage message =
            case maybeLastContext of
                Just lastContext ->
                    String.join "\n\n"
                        [ "✗ " ++ lastContext, message ]

                Nothing ->
                    message

        outputContext =
            otherContexts
                |> List.map ((++) "↓ ")
                |> String.join "\n"
    in
        outputContext ++ "\n" ++ message


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
