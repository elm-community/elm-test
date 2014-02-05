module Main where

import Test
import ElmTest.Runner.Element as Element
import ElmTest.Runner.String  as String

prettyOut : Element
prettyOut = Element.runDisplay Test.tests

uglyOut : String
uglyOut = snd <| String.runDisplay Test.tests

main : Element
main = above (plainText uglyOut) prettyOut
