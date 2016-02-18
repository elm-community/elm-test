module ElmTest.Runner.Element (runDisplay) where

{-| Run a test suite and display it as an Element

# Run
@docs runDisplay

-}

import Color
import Graphics.Element exposing (..)
import List exposing ((::))
import List
import String
import Text
import ElmTest.Run as Run
import ElmTest.Test exposing (..)
import ElmTest.Runner.String as StringRunner


plainText : String -> Element
plainText s =
  leftAligned (Text.fromString s)


red : Color.Color
red =
  Color.rgb 255 126 132


-- Given a result, render it in plainText and return a pass/fail color
pretty : ( String, Run.Result ) -> Element
pretty ( s, result ) =
  let
    w =
      indent s * 10

    w' =
      5
  in
    case result of
      Run.Pass _ ->
        flow right [ spacer w 1, plainText s, spacer w' 1 ]

      Run.Fail _ _ ->
        color red <| flow right [ spacer w 1, plainText s, spacer w' 1 ]

      Run.Report _ _ ->
        let
          f =
            if Run.failedTests result > 0 then
              color red
            else
              identity
        in
          f <| flow right [ spacer w 1, leftAligned << Text.bold << Text.fromString <| s, spacer w' 1 ]


indent : String -> Int
indent s =
  let
    trimmed =
      String.trimLeft s
  in
    String.length s - String.length trimmed


maxOrZero : List Int -> Int
maxOrZero l =
  List.foldl max 0 l


{-| Runs a list of tests and renders the results as an Element
-}
runDisplay : Test -> Element
runDisplay tests =
  case StringRunner.run tests of
    ( summary, allPassed ) :: results ->
      let
        results' =
          List.map pretty results

        maxWidth =
          maxOrZero << List.map widthOf <| results'

        separator =
          spacer maxWidth 1 |> color Color.white

        elements =
          if results == [ ( "", allPassed ) ] then
            []
          else
            results'
            |> List.map (\elem -> width maxWidth elem)
            |> List.intersperse separator
      in
        flow down
          <| plainText summary
          :: spacer 1 10
          :: elements

    _ ->
      flow down []
