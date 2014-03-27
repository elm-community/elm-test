module Main where

import IO.IO (..)
import IO.Runner as Run

import Test
import ElmTest.Runner.Console as Console

sigs : IO ()
sigs = Console.runDisplay Test.tests

-- | Can't use a type alias in ports, yet :/
port requests : Signal [{ mPut  : Maybe String
                        , mExit : Maybe Int
                        , mGet  : Bool
                        }]
port requests = Run.run responses sigs

port responses : Signal (Maybe String)