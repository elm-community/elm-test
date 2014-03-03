module Main where

import ElmTest.Runner.Console (runDisplay)
import open ElmTest.Test

tests :: [Test]
tests = [ 5 `equals` 5
        , 10 `equals` 10
        ]

console : IO ()
console = runDisplay tests