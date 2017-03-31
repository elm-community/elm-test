module Main exposing (..)

{-| HOW TO RUN THESE TESTS

$ npm test

Note that this always uses an initial seed of 902101337, since it can't do effects.
-}

import Runner.Log
import Platform
import Tests


main : Program Never () msg
main =
    Platform.program
        { init = ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
        |> Runner.Log.run Tests.all
