module ElmTest where

import open Either

-- Results
----------

-- The type representing a test result. The Right string is just "Pass" and the Left string
-- contains the error message in the event of a failure
type Result = Either String String

-- Running Tests
----------------

-- Function to run a test and get a result
run : Test -> Result
run (TestCase name assertion) = 
    case assertion of
        AssertEqual a b    -> if (a == b)
                                  then Right "Pass"
                                  else Left <| "Expected: " ++ (show b) ++ "; got: " ++ (show a)
        AssertNotEqual a b -> if (a /= b)
                                  then Right "Pass"
                                  else Left <| (show a) ++ " equals " ++ (show b)
        AssertTrue a       -> if (a)
                                  then Right "Pass"
                                  else Left <| (show a) ++ " is not True"
        AssertFalse a      -> if (not a)
                                  then Right "Pass"
                                  else Left <| (show a) ++ " is not False"

-- Function to run multiple tests and get their results
runTests : [Test] -> [Result]
runTests = map run

-- Pretty Output
----------------

-- Given a result, render it in plainText and return a pass/fail color 
pretty : Result -> (Color, Element)
pretty result =
    case result of
        Right str -> (green, plainText str)
        Left  str -> (red, plainText str)
        
-- A wrapper around both runTests and pretty, runs a list of tests and renders the results        
runPrettyTests : [Test] -> Element
runPrettyTests tests =
    let results  = runTests tests
        pretties = map pretty results
        w        = (maximum <| map (\r -> widthOf <| snd r) pretties) + 20
        passed   = length <| rights results
        failed   = length <| lefts results in
    (flow right <| [ text . bold . toText <| (show (length results)) ++ " tests executed: "
                   , text . Text.color green . toText <| (show passed) ++ " passed; "
                   , text . Text.color red . toText <| (show failed) ++ " failed"
                   ])
    `above`
    (flow right <| [ flow down <| map (\t -> plainText <| (name t) ++ ":   ") tests
                   , flow down <| 
                        map (\(c, t) -> color c <| container w (heightOf t) middle t) pretties ])
