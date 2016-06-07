module Test.Runner.Log exposing (run)

import Test exposing (Test)
import Html
import Html.App
import Random.Pcg as Random
import Assert exposing (Outcome)
import String


{-| Artisanal handcrafted seed with certified organic entropy.

(This runner can't run tasks, so we can't access a better initial seed.)
-}
randomSeed : Random.Seed
randomSeed =
    Random.initialSeed 42


toOutput : (() -> Outcome) -> ( String, Int ) -> ( String, Int )
toOutput thunk ( output, failureCount ) =
    case Assert.toFailures (thunk ()) of
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


run : Test -> Program Never
run test =
    let
        runners =
            Test.toRunners randomSeed test

        ( output, failureCount ) =
            List.foldl toOutput ( "", 0 ) runners

        _ =
            if failureCount > 0 then
                output
                    ++ (toString failureCount ++ " TESTS FAILED!\n\nExit code")
                    |> (flip Debug.log 1)
                    |> (\_ -> Debug.crash "FAILED TEST RUN")
                    |> (\_ -> ())
            else
                output
                    ++ "ALL TESTS PASSED!\n\nExit code"
                    |> (flip Debug.log 0)
                    |> (\_ -> ())
    in
        Html.App.beginnerProgram
            { model = ()
            , update = \_ model -> model
            , view = \_ -> Html.text ""
            }
