module Main exposing (..)

{-| HOW TO RUN THESE TESTS

$ npm test

Note that this always uses an initial seed of 902101337, since it can't do effects.

-}

import Platform
import Runner.Log
import Runner.String exposing (Summary)
import SeedTests
import Tests


main : Program Never () msg
main =
    let
        program =
            Platform.program
                { init = ( (), Cmd.none )
                , update = \_ _ -> ( (), Cmd.none )
                , subscriptions = \_ -> Sub.none
                }
    in
    runAllTests program


runAllTests : a -> a
runAllTests a =
    let
        runSeedTest =
            Runner.String.runWithOptions 1 SeedTests.fixedSeed

        _ =
            [ [ Runner.String.run Tests.all ]
            , List.map runSeedTest SeedTests.tests
            , List.map (runSeedTest >> removeAutoFail) SeedTests.noAutoFail
            ]
                |> List.concat
                |> List.foldl combineSummaries emptySummary
                |> Runner.Log.logOutput
    in
    a


emptySummary : Summary
emptySummary =
    { output = "", passed = 0, failed = 0, autoFail = Nothing }


{-| Considers autoFail as pass so we can actually write tests about Test.skip
and Test.only which do not automatically fail.
-}
removeAutoFail : Summary -> Summary
removeAutoFail summary =
    { summary | autoFail = Nothing }


combineSummaries : Summary -> Summary -> Summary
combineSummaries first second =
    { output = first.output ++ second.output
    , passed = first.passed + second.passed
    , failed = first.failed + second.failed
    , autoFail =
        case ( first.autoFail, second.autoFail ) of
            ( Nothing, Nothing ) ->
                Nothing

            ( Nothing, second ) ->
                second

            ( first, Nothing ) ->
                first

            ( Just first, Just second ) ->
                [ first, second ]
                    |> String.join "\n"
                    |> Just
    }
