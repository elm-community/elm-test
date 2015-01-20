module ElmTest.Runner.Element (runDisplay) where

{-| Run a test suite and display it as an Element

# Run
@docs runDisplay

-}

import Color (..)
import Graphics.Element (..)
import List ((::))
import List
import String
import Text

import ElmTest.Run as Run
import ElmTest.Test (..)
import ElmTest.Runner.String as String

plainText : String -> Element
plainText s = Text.leftAligned (Text.fromString s)

-- Given a result, render it in plainText and return a pass/fail color
pretty : (String, Run.Result) -> Element
pretty (s, result) =
    let w = indent s * 10
        w' = 5
    in  case result of
            Run.Pass _   -> color green <| flow right [spacer w 1, plainText s, spacer w' 1]
            Run.Fail _ _ -> color red <| flow right [spacer w 1, plainText s, spacer w' 1]
            Run.Report _ _ -> let c = if Run.failedTests result > 0 then red else green
                              in  color c <| flow right [spacer w 1, Text.leftAligned << Text.bold << Text.fromString <| s, spacer w' 1]
indent : String -> Int
indent s = let trimmed = String.trimLeft s
           in  String.length s - String.length trimmed

maxOrZero : List Int -> Int
maxOrZero l =
    List.foldl max 0 l

{-| Runs a list of tests and renders the results as an Element -}
runDisplay : Test -> Element
runDisplay tests =
    let ((summary, allPassed) :: results) = String.run tests
        results' = List.map pretty results
        maxWidth = maxOrZero << List.map widthOf <| results'
        maxHeight = maxOrZero << List.map heightOf <| results'
        elements = if results == [("", allPassed)]
                   then []
                   else List.map (color black << container (maxWidth + 2) (maxHeight + 2) midLeft << width maxWidth) results'
    in  flow down <| plainText summary :: spacer 1 10 :: elements
