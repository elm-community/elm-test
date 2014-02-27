Elm-Test
========

A unit testing framework for Elm

## Creating Tests

Creating a test case is very simple. You only need a name and an assertion:
```haskell
myTest = test "Example Test" (assert True)
```
For convenience, there is a function to create a name for you based on the inputs:
```haskell
-- Test name will be "5 == 5"
myTest = defaultTest (assertEqual 5 5)
```
There are four different types of assertions:
```haskell
AssertTrue
AssertFalse
AssertEqual
AssertNotEqual
```
As well as functions for making these assertions:
```haskell
assert : Bool -> Assertion
assertEqual : a -> a -> Assertion
assertNotEqual : a -> a -> Assertion
assertionList : [a] -> [a] -> [Assertion]
```
Example usage of these functions might be:
```haskell
assert        (a > 5)             -- Returns an AssertTrue assertion
assertEqual    a b                -- Returns an AssertEqual assertion
assertNotEqual a b                -- Returns an AssertNotEqual assertion
assertionList [a, b, c] [d, e, f] -- Shorthand for [assertEqual a d, assertEqual b e, assertEqual c f]
```

## Running Tests

Running a test produces a result. A result is either a pass or a failure, so it is represented as `Maybe String`. `Nothing` denotes the test passed, and `Just reason` indicates the test failed, and gives a `String` with a simple failure message.

The most basic way to run a test is the `run` function, which has the type signature `Test -> Result`.

A list of tests can be run all at once, producing a `Report` Given a list of tests `myTests`, you can write: `report myTests`.

A `Report` is of type `{results : [Result], passes : [Result], failures : [Result]}`.
There is no built-in way to display results or reports, but there are functions for running tests and immediately seeing the results. 

## Displaying Results

In `ElmTest.Runner.Element` lives `runDisplay : [Test] -> Element`, which is an easy way to run your tests and report the results in-browser, as a standard Elm module. A full example could be:
```haskell
-- Example.elm
import String

import ElmTest.Test (test, Test)
import ElmTest.Assertion (assert, assertEqual)
import ElmTest.Runner.Element (runDisplay)

tests : [Test]
tests = [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert False)
        ]

main : Element
main = runDisplay tests
```
Compile this with `elm --make Example.elm` and open the resulting `build/Example.html` file in your browser, and you'll see the results.

Another method is the `runDispay : [Test] -> (Bool, String)` function in `ElmTest.Runner.String`. This is almost the same, but it returns a `(Bool, String)` instead of an `Element`. The `Bool` is `True` if all tests passed, otherwise it is `False`. The `String` is a summary of the overall test results. Here's the same example as before, but modified for `ElmTest.Runner.String`:
```haskell
-- Example.elm
import String

import ElmTest.Test (test, Test)
import ElmTest.Assertion (assert, assertEqual)
import ElmTest.Runner.String (runDisplay)

tests : [Test]
tests = [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert False)
        ]

results : String
results = snd <| runDisplay tests

main : Element
main = plainText results
```

There is one more version of this function. `runDisplay : [Test] -> {stdout : Signal String, exit : Signal (Maybe Int)}` which lives in `ElmTest.Runner.Console`. This is designed to work with [Max New's Elm IO library](https://github.com/maxsnew/IO/). Documentation on how to do this will appear soon!

## Demo

For a quick demo, you can compile the `ElementExample.elm` file, or continue to the next section:

## Testing from the Command Line

This repo includes a shell script that automates the process of
creating a script for testing elm from the command line without using
a web browser.

To run the example:
```bash
$ ./mkScript.sh ScriptExample
$ node ScriptExample.js
  4 tests executed
  3 tests passed
  1 tests failed
8 == 1: FAILED. Expected: 8; got: 1
3 == 3: passed.
True: passed.
test head: passed.
```

and exits with return code 1.
