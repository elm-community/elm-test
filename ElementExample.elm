module Main where

import Test
import ElmTest.Run as Run
import ElmTest.Runner.Element as Element
import ElmTest.Runner.String  as String

prettyOut : Element
prettyOut = Element.runDisplay Test.suite2

uglyOut : String
uglyOut = snd <| String.runDisplay Test.suite2

main : Element
main = above (plainText uglyOut) prettyOut
