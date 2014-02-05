module ElmTest.Test where

import open ElmTest.Assertion

{-| The units of a test suite, named tests.

# Test
@docs test, (~=?), defaultTest, Test
 -}

-- Basic test data constructor
data Test = TestCase String Assertion

{- Convenience operator for quickly constructing Assert Equals tests. -}
(~=?) : a -> a -> Test
a ~=? b = defaultTest <| assertEqual a b

{- Basic function to create a Test Case -}
test : String -> Assertion -> Test
test name a = TestCase name a

{- Automatically determines a name for the created test (use this if you're lazy). -}
defaultTest : Assertion -> Test
defaultTest a =
    let name = case a of
                 AssertTrue _ -> "True"
                 AssertTrue _ -> "False"
                 AssertEqual _ a b    -> a ++ " == " ++ b
                 AssertNotEqual _ a b -> a ++ " /= " ++ b
    in test name a

-- -- Given a list of values and another of expected values, generate a list of test cases
-- testList : [String] -> [a] -> [a] -> [Test]
-- testList names xs ys = map (\(name, ass) -> if (name == []) then defaultTest ass else test name ass) <| 
--                            zip names <| assertionList xs ys

-- -- Given a function name, a unary function, a list of inputs, and a list of expected outputs,
-- -- generate a list of test cases
-- testFunction : String -> (a -> b) -> [(a, b)] -> [Test]
-- testFunction name' f args =
--     let inputs     = map fst args
--         expecteds  = map snd args
--         outputs    = map f inputs
--         names      = map (\n -> name' ++ " " ++ (show n)) inputs in
--     testList names outputs expecteds

-- -- Like testFunction, but tests a binary function
-- testFunction2 : String -> (a -> b -> c) -> [((a, b), c)] -> [Test]
-- testFunction2 name' f args =
--     let inputs = map fst args
--         expecteds = map snd args
--         outputs = map (uncurry f) inputs
--         names = map (\n -> name' ++ " " ++ (show n)) inputs in
--     testList names outputs expecteds

