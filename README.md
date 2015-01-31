Elm-Test [![Build Status](https://travis-ci.org/deadfoxygrandpa/Elm-Test.png?branch=master)](https://travis-ci.org/deadfoxygrandpa/Elm-Test)
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
As well as a function to create an `assertEqual` tests, again deriving a name based on the inputs:
```haskell
myTest = defaultTest (5 `assertEqual` 5)
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
assertionList : List a -> List a -> List Assertion
```
Example usage of these functions might be:
```haskell
assert        (a > 5)             -- Returns an AssertTrue assertion
assertEqual    a b                -- Returns an AssertEqual assertion
assertNotEqual a b                -- Returns an AssertNotEqual assertion
assertionList [a, b, c] [d, e, f] -- Shorthand for [assertEqual a d, assertEqual b e, assertEqual c f]
```

## Grouping Tests

Writing many tests as a flat list quickly becomes unwieldy. For easier maintenance you can group tests into logical
units called test suites. The following function will create a test suite from a suite name and a list of tests:
```haskell
suite : String -> List Test -> Test
```
The type of a test suite is simply `Test`, allowing use of all the test runners with either a single test or a suite of tests. Test suites can also contain subsuites, of course.

The other benefit of grouping tests into suites is that the test runners described in the following sections will greatly simplify the output, showing only detailed information in suites that contain failed tests, making it easier to quickly spot the failures instead of being flooded with irrelevant data.

## Running Tests

Running a test produces a result. A result is either a pass, a failure, or a report containing detailed results from
the tests or subsuites contained in a suite. All results contain the name of the test or suite that was run, and failures additionally contain a failure message giving a hint as to why the test failed.

The most basic way to run a test is the `run` function, which has the type signature `Test -> Result`. A test suite can also be run all at once, again with the `run` function.

A `Report` is of type `{results : List Result, passes : List Result, failures : List Result}`.
There is no built-in way to display results, but there are functions for running tests and immediately seeing the results. 

## Displaying Results

In `ElmTest.Runner.Element` lives `runDisplay : Test -> Element`, which is an easy way to run your tests and report the results in-browser, as a standard Elm module. A full example could be:
```haskell
-- Example.elm
import String

import ElmTest.Test (test, Test, suite)
import ElmTest.Assertion (assert, assertEqual)
import ElmTest.Runner.Element (runDisplay)

tests : Test
tests = suite "A Test Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert False)
        ]

main : Element
main = runDisplay tests
```
Compile this with `elm-make Example.elm --output Example.html` and open the resulting file in your browser, and you'll see the results.

Another method is the `runDispay : Test -> String` function in `ElmTest.Runner.String`. This is almost the same, but it returns a `String` instead of an `Element`. The `String` is a summary of the overall test results. Here's the same example as before, but modified for `ElmTest.Runner.String`:
```haskell
-- Example.elm
import String

import ElmTest.Test (test, Test)
import ElmTest.Assertion (assert, assertEqual)
import ElmTest.Runner.String (runDisplay)

tests : Test
tests = suite "A Test Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert False)
        ]

results : String
results = runDisplay tests

main : Element
main = plainText results
```

There is one more version of this function. `runDisplay : Test -> IO ()` which lives in `ElmTest.Runner.Console`. This is designed to work with [Max New's Elm IO library](https://github.com/maxsnew/IO/). See the below section on **Testing from the Command Line** for details.

## Demo

For a quick demo, you can compile the `ElementExample.elm` file, or continue to the next section:

## Testing from the Command Line
See https://github.com/maxsnew/IO for details, but here's the short version:
Make a file that uses the `IO` runner and sets up the appropriate ports:
```
-- Example.elm
import String

import IO.IO (..)
import IO.Runner (Request, Response, run)
import ElmTest.Test (test, Test)
import ElmTest.Assertion (assert, assertEqual)
import ElmTest.Runner.Console (runDisplay)

tests : Test
tests = suite "A Test Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert False)
        ]

port requests : Signal Request
port requests = run responses (runDisplay tests)

port responses : Signal Responseresults : String
```
Then download the `elm-io.sh` script and `jsdom` to run it:
(On Windows, `jsdom` is somewhat difficult to install. [Refer to this blog post](http://www.steveworkman.com/node-js/2012/installing-jsdom-on-windows/) for detailed instructions)
```bash
$ npm install jsdom
...
$ curl https://raw.githubusercontent.com/maxsnew/IO/master/elm-io.sh > elm-io.sh
$ elm-make Example.elm --output raw-test.js
$ bash elm-io.sh raw-test.js test.js
$ node test.js
  5 suites run, containing 17 tests
  3 suites and 16 tests passed
  2 suites and 1 tests failed

Test Suite: A Test Suite: FAILED
  Test Suite: Some tests: all tests passed
  Test Suite: Some other tests: FAILED
    8 == 1: FAILED. Expected: 8; got: 1
    3 == 3: passed.
    True: passed.
    test head: passed.
  Test Suite: More tests!: all tests passed
  3 == 3: passed.
  Test Suite: Even more!!: all tests passed
```
While the `Element` display is nicest to read, the `IO` runner is amenable to automated testing. If a test suite passes the script will exit with exit code 0, and if it fails it will exit with 1.

## Integrating With Travis CI

With Elm-Test and IO, it is now possible to run continuous integration tests with Travis CI on
your Elm projects. Just set up Travis CI for your repository as normal, write tests with Elm-Test,
and include a `.travis.yml` file based on the following:
```
language: haskell
install:
  - cabal install elm-make
  - cabal install elm-package
  - curl https://raw.githubusercontent.com/maxsnew/IO/master/elm-io.sh > elm-io.sh
  - npm install jsdom
  - elm-package install -y
before_script: 
  - elm-make --yes --output raw-test.js Tests/Tests.elm
  - bash elm-io.sh raw-test.js test.js
script: node test.js
```
For convenience, we've also uploaded precompiled binaries based on the official Elm 0.14 release and a setup script to [http://deadfoxygrandpa.github.io/elm-travis-cache](https://github.com/deadfoxygrandpa/elm-travis-cache/tree/gh-pages) which is being used in the `.travis.yml` in this repository. With this script, the previous `.travis.yml` example can be reduced to:
```
language: haskell
install:
- wget http://deadfoxygrandpa.github.io/elm-travis-cache/elm-test-install.sh
- bash elm-test-install.sh
before_script:
- ./elm-make --yes --output raw-test.js Tests/Tests.elm
- bash elm-io.sh raw-test.js test.js
script: node test.js
```

