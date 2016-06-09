module Test.Runner.Log exposing (run)

import Suite exposing (Suite)
import Html
import Html.App
import Random.Pcg as Random
import Test exposing (Test)
import String


{-| Artisanal handcrafted seed with certified organic entropy.

(This runner can't run tasks, so we can't access a better initial seed.)
-}
randomSeed : Random.Seed
randomSeed =
    Random.initialSeed 42


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


run : Suite -> Program Never
run suite =
    let
        runners =
            Suite.toRunners randomSeed suite

        ( output, failureCount ) =
            List.foldl toOutput ( "", 0 ) runners

        _ =
            if failureCount > 0 then
                output
                    ++ (toString failureCount ++ " SuiteS FAILED!\n\nExit code")
                    |> (flip Debug.log 1)
                    |> (\_ -> Debug.crash "FAILED Suite RUN")
                    |> (\_ -> ())
            else
                output
                    ++ "ALL SuiteS PASSED!\n\nExit code"
                    |> (flip Debug.log 0)
                    |> (\_ -> ())
    in
        Html.App.beginnerProgram
            { model = ()
            , update = \_ model -> model
            , view = \_ -> Html.text ""
            }
