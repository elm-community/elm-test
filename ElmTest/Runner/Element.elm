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
      Nothing  -> (green, plainText "Pass.")
      Just msg -> (red,   plainText msg)
        
{-| Runs a list of tests and renders the results as an Element -}
runDisplay : [Test] -> Element
runDisplay tests =
    let r        = report tests
        pretties = map pretty r.results
        w        = (maximum <| map (\r -> widthOf <| snd r) pretties) + 20
        passed   = length r.passes
        failed   = length r.failures
        name (TestCase n _) = n
    in
    (flow right <| [ text . bold . toText <| (show (length r.results)) ++ " tests executed: "
                   , text . Text.color green . toText <| (show passed) ++ " passed; "
                   , text . Text.color red . toText <| (show failed) ++ " failed"
                   ])
    `above`
    (flow right <| [ flow down <| map (\t -> plainText <| (name t) ++ ":   ") tests
                   , flow down <| 
                        map (\(c, t) -> color c <| container w (heightOf t) middle t) pretties ])
