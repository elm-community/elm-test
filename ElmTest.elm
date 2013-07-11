module ElmTest where

import open Either

-- Tests
--------

-- Basic test data constructor
data Test = TestCase String Assertion

-- Utility function for getting the name of a Test Case
name : Test -> String
name (TestCase name _) = name

-- Convenience operator for quickly constructing Assert Equals tests
(~=?) : a -> a -> Test
a ~=? b = defaultTest <| assertEqual a b

-- Basic function to create a Test Case
test : String -> Assertion -> Test
test name a = TestCase name a

-- Automatically determines a name for the created test (use this if you're lazy)    
defaultTest : Assertion -> Test
defaultTest a =
    let name = case a of
                   AssertEqual a b    -> (show a) ++ " == " ++ (show b)
                   AssertNotEqual a b -> (show a) ++ " /= " ++ (show b)
                   AssertTrue a       -> (show a) ++ " is True"
                   AssertFalse a      -> (show a) ++ " is False" in
    test name a

-- Given a list of values and another of expected values, generate a list of test cases
testList : [String] -> [a] -> [a] -> [Test]
testList names xs ys = map (\(name, ass) -> if (name == []) then defaultTest ass else test name ass) <| 
                           zip names <| assertionList xs ys

-- Given a function name, a function, a list of inputs, and a list of expected outputs,
-- generate a list of test cases
generateFunctionTests : String -> (a -> b) -> [a] -> [b] -> [Test]
generateFunctionTests name' f inputs expecteds =
    let outputs    = map f inputs
        names      = map (\n -> name' ++ " " ++ (show n)) inputs in
    testList names outputs expecteds

-- Assertions
-------------

-- The fundamental component of a Test Case
data Assertion a = AssertEqual a a | AssertNotEqual a a | AssertTrue a | AssertFalse a 

-- Convenience operator for quickly constructing an Assert Equals assertion
(@=?) : a -> a -> Assertion
a @=? b = assertEqual a b

-- Convenience operator for quickly constructing an Assert Not Equals assertion
(@/=?) : a -> a -> Assertion
a @/=? b = assertNotEqual a b

-- Basic function to create an Assert True assertion
assert : a -> Assertion
assert a = AssertTrue a

-- Basic function to create an Assert Equals assertion
assertEqual : a -> a -> Assertion
assertEqual a b = AssertEqual a b

-- Given a list of values and another list of expected values, generate a list of
-- Assert Equal assertions
assertionList : [a] -> [a] -> [Assertion]
assertionList xs ys = map (\(a, b) -> assertEqual a b) <| zip xs ys

-- Basic function to create an Assert Not Equals assertion
assertNotEqual : a -> a -> Assertion
assertNotEqual a b = AssertNotEqual a b

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

-- Example Usage
----------------

main = runPrettyTests <| [ (2^3) ~=? 1
                         , 3 ~=? 3
                         , defaultTest (True @=? True)
                         , test "test head" (assertEqual 1 (head [1..10]))
                         ]
                         ++
                         (generateFunctionTests "Square" 
                                                (\x -> if (x < 4) then x^2 else x^3)
                                                [1..5]
                                                [1,4,9,16,25])