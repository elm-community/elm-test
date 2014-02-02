module ScriptExample where

import Test
import ElmTest.Runner.Console as Console

sigs : Console.ConsoleSignals
sigs = Console.runDisplay Test.tests

port stdout : Signal String
port stdout = sigs.stdout

port exit : Signal (Maybe Int)
port exit = sigs.exit
