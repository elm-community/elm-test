module Test.Runner.String exposing (run, runWithOptions)

import Random.Pcg as Random
import Test exposing (Test, Suite)
import Test.Outcome exposing (Outcome)
import String
import Test.Runner exposing (toRunners)


toOutput : (() -> ( List String, Outcome )) -> ( String, Int ) -> ( String, Int )
toOutput thunk ( output, failureCount ) =
    let
        ( labels, outcome ) =
            thunk ()
    in
        case Test.Outcome.toFailures outcome of
            Just failures ->
                ( String.join "\n\n" (output :: List.map (outputFailures labels) failures)
                , failureCount + 1
                )

            Nothing ->
                ( output, failureCount )


outputFailures : List String -> String -> String
outputFailures labels message =
    let
        ( maybeLastContext, otherContexts ) =
            case List.reverse labels of
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


run : Suite -> ( String, Int )
run =
    runWithOptions defaultSeed 100


runWithOptions : Random.Seed -> Int -> Suite -> ( String, Int )
runWithOptions seed runs suite =
    suite
        |> toRunners seed runs
        |> List.foldl toOutput ( "", 0 )
