module LogRunnerExample exposing (..)

{-| HOW TO RUN THIS EXAMPLE

$ elm-make LogRunnerExample.elm --output=elm.js
$ node elm.js

Note that this always uses an initial seed of 42, since it can't do effects.
-}

import Assert
import Test exposing (..)
import Test.Runner.Log
import Html.App
import Html


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Check the console for useful output!"
        }
        |> Test.Runner.Log.run testOxfordify


{-| stubbed function under test
-}
oxfordify : a -> b -> c -> String
oxfordify _ _ _ =
    "Alice, Bob, and Claire"


testOxfordify : Test
testOxfordify =
    describe "oxfordify"
        [ describe "given an empty sentence"
            [ test "returns an empty string" <|
                \() ->
                    oxfordify "This sentence is empty" "." []
                        |> Assert.equal ""
            ]
        , describe "given a sentence with one item"
            [ test "still contains one item" <|
                \() ->
                    oxfordify "This sentence contains " "." [ "one item" ]
                        |> Assert.equal "This sentence contains one item."
            ]
        , describe "given a sentence with multiple items"
            [ test "returns an oxford-style sentence" <|
                \() ->
                    oxfordify "This sentence contains " "." [ "one item", "two item" ]
                        |> Assert.equal "This sentence contains one item and two item."
            , test "returns an oxford-style sentence" <|
                \() ->
                    oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
                        |> Assert.equal "This sentence contains one item, two item, and three item."
            ]
        , describe "a long failure message"
            [ test "long failure!" <|
                \() ->
                    Assert.equal "html, body {\nwidth: 100%;\nheight: 100%;\nbox-sizing: border-box;\npadding: 0;\nmargin: 0;\n}\n\nbody {\nmin-width: 1280px;\noverflow-x: auto;\n}\n\nbody > div {\nwidth: 100%;\nheight: 100%;\n}\n\n.dreamwriterHidden {\ndisplay: none !important;\n}\n\n#dreamwriterPage {\nwidth: 100%;\nheight: 100%;\nbox-sizing: border-box;\nmargin: 0;\npadding: 8px;\nbackground-color: rgb(100, 90, 128);\ncolor: rgb(40, 35, 76);\n}"
                        "html, body {\nwidth: 100%;\nheight: 100%;\nbox-sizing: border-box;\npadding: 0;\nmargin: 0;\n}\n\nbody {\nmin-width: 1280px;\noverflow-x: auto;\n}\n\nbody > div {\nwidth: 100%;\nheight: 100%;\n}\n\n.dreamwriterHidden {\ndisplay: none !important;\n}\n\n#Page {\nwidth: 100%;\nheight: 100%;\nbox-sizing: border-box;\nmargin: 0;\npadding: 8px;\nbackground-color: rgb(100, 90, 128);\ncolor: rgb(40, 35, 76);\n}"
            ]
        ]
