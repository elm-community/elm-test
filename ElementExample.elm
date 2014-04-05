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
main = flow down [ plainText <| uglyOut ++ "\n\n\n\n"
                 , plainText <| (snd <| String.runDisplay Test.passingTest) ++ "\n\n\n\n"
                 , plainText <| (snd <| String.runDisplay Test.failingTest) ++ "\n\n\n\n"
                 , prettyOut
                 ]
