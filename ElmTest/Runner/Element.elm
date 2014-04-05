module ElmTest.Runner.Element (runDisplay) where

{-| Run a test suite and display it as an Element

# Run
@docs runDisplay

-}

import ElmTest.Run (..)
import ElmTest.Test (..)

-- Given a result, render it in plainText and return a pass/fail color
pretty : Result -> (Color, Element)
pretty m =
    case m of
      Pass _  -> (green, plainText "Pass.")
      Fail _ msg -> (red,   plainText msg)
      Report _ {results, passes, failures} -> (purple, plainText "report")

{-| Runs a list of tests and renders the results as an Element -}
runDisplay : Test -> Element
runDisplay tests =
    let r        = case run tests of
                      Report _ r -> r
                      _        -> {results = [], passes = [], failures = []}
        pretties = map pretty r.results
        w        = (maximum <| map (\r -> widthOf <| snd r) pretties) + 20
        passed   = length r.passes
        failed   = length r.failures
        name test = case test of
                        TestCase n _ -> n
                        Suite n _ -> n
    in
    (flow right <| [ centered . bold . toText <| (show (length r.results)) ++ " tests executed: "
                   , centered . Text.color green . toText <| (show passed) ++ " passed; "
                   , centered . Text.color red . toText <| (show failed) ++ " failed"
                   ])
    `above`
    (flow right <| [ flow down <| map (\t -> plainText <| (name t) ++ ":   ") [tests]
                   , flow down <|
                        map (\(c, t) -> color c <| container w (heightOf t) middle t) pretties ])
