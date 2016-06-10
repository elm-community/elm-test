module Test.Runner.String exposing (run)

import Suite exposing (Suite)
import Random.Pcg as Random
import Test exposing (Test)
import String


toOutput : (() -> Test) -> ( String, Int ) -> ( String, Int )
toOutput thunk ( output, failureCount ) =
    case Test.toFailures (thunk ()) of
        Just failures ->
            ( String.join "\n\n" [ output, outputFailures failures ]
            , failureCount + 1
            )

        Nothing ->
            ( output, failureCount )


outputFailures : { messages : List String, context : List String } -> String
outputFailures { messages, context } =
    let
        ( maybeLastContext, otherContexts ) =
            case List.reverse context of
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
        (outputContext :: List.map outputMessage messages)
            |> String.join "\n"


run : Random.Seed -> Suite -> ( String, Int )
run seed suite =
    suite
        |> Suite.toRunners seed
        |> List.foldl toOutput ( "", 0 )
