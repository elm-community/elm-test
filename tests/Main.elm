module Main exposing (..)

{-| HOW TO RUN THESE TESTS

$ elm-make Main.elm --output=elm.js
$ node elm.js

Note that this always uses an initial seed of 902101337, since it can't do effects.
-}

import Runner.Log
import Html.App
import Html
import Tests


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Check the console for useful output!"
        }
        |> Runner.Log.run Tests.all
