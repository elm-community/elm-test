module Main where

import Test
import Text
import Graphics.Element exposing (..)
import ElmTest.Run as Run
import ElmTest.Runner.Element as ElementRunner
import ElmTest.Runner.String  as StringRunner

plainText : String -> Element
plainText s = leftAligned (Text.fromString s)

prettyOut : Element
prettyOut = ElementRunner.runDisplay Test.suite3

uglyOut : String
uglyOut = StringRunner.runDisplay Test.suite2

uglyOut' : String
uglyOut' = StringRunner.runDisplay Test.suite3

main : Element
main = flow down [ plainText "All tests in this suite will pass:\n\n"
                 , plainText uglyOut
                 , plainText "\nThis suite has a failing test:\n\n"
                 , plainText uglyOut'
                 , plainText "\nThe Element runner:\n\n"
                 , prettyOut
                 ]
