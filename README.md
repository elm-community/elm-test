elm-test [![Build Status](https://travis-ci.org/deadfoxygrandpa/elm-test.png?branch=master)](https://travis-ci.org/deadfoxygrandpa/elm-test)
========

A unit testing framework for Elm

## Getting Started

The simplest way to get started with Elm Test is to install & run it via [node-elm-test](https://github.com/rtfeldman/node-elm-test). This package can install Elm Test and its dependencies for you, as well as providing you with a command line test runner and an example test suite.

## Creating Tests

Creating a test case is very simple. You only need a name and an assertion:
```elm
myTest = test "Example Test" (assert True)
```
For convenience, there is a function to create a name for you based on the inputs:
```elm
-- Test name will be "5 == 5"
myTest = defaultTest (assertEqual 5 5)
```
As well as a function to create an `assertEqual` tests, again deriving a name based on the inputs:
```elm
myTest = defaultTest (5 `assertEqual` 5)
```
There are five different functions to create assertions:
```elm
assert : Bool -> Assertion
assertEqual : a -> a -> Assertion
assertNotEqual : a -> a -> Assertion
lazyAssert : (() -> Bool) -> Assertion 
assertionList : List a -> List a -> List Assertion
```
Example usage of these functions might be:
```elm
assert        (a > 5)             -- Returns an AssertTrue assertion
assertEqual    a b                -- Returns an AssertEqual assertion
assertNotEqual a b                -- Returns an AssertNotEqual assertion
lazyAssert (\_ -> a > 5)          -- Same as the assert example, but delays execution until test runtime
assertionList [a, b, c] [d, e, f] -- Shorthand for [assertEqual a d, assertEqual b e, assertEqual c f]
```
The `lazyAssert` function can be useful for testing functions which might possibly cause a runtime error. With all the
other assertion functions, the tests are actually run when the file is loaded, which can cause runtime errors
on page load, but with `lazyAssert`, any runtime errors are delayed until actual test execution. Note that for this to
work, you must manually write an anonymous function of type `() -> Bool`;

## Grouping Tests

Writing many tests as a flat list quickly becomes unwieldy. For easier maintenance you can group tests into logical
units called test suites. The following function will create a test suite from a suite name and a list of tests:
```elm
suite : String -> List Test -> Test
```
The type of a test suite is simply `Test`, allowing use of all the test runners with either a single test or a suite of
tests. Test suites can also contain subsuites, of course.

The other benefit of grouping tests into suites is that the test runners described in the following sections will greatly
simplify the output, showing only detailed information in suites that contain failed tests, making it easier to quickly spot
the failures instead of being flooded with irrelevant data.

## Running Tests

The simplest way to run tests and display the output is the `elementRunner : Test -> Element` function, which is an easy way
to run your tests and report the results in-browser, as a standard Elm module. A full example could be:
```elm
-- Example.elm
import String
import Graphics.Element exposing (Element)

import ElmTest exposing (..)


tests : Test
tests = 
    suite "A Test Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert False)
        ]


main : Element
main = 
    elementRunner tests
```
Compile this with `elm-make Example.elm --output Example.html` and open the resulting file in your browser, and you'll see
the results.

Another method is the `stringRunner : Test -> String` function. This is almost the same, but it returns a `String` instead of
an `Element`. The `String` is a summary of the overall test results. Here's the same example as before, but modified for
`stringRunner`:
```elm
-- Example.elm
import String
import Graphics.Element exposing (Element, show)

import ElmTest exposing (..)


tests : Test
tests = 
    suite "A Test Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert False)
        ]


main : Element
main = 
    show (stringRunner tests)
```

There is one more version of this function: `consoleRunner : Test -> IO ()`. This is designed to work with 
[Laszlo Pandy's elm-console library](https://github.com/laszlopandy/elm-console/). See the below section on 
**Testing from the Command Line** for details.

## Demo

For a quick demo, you can compile the `ElementExample.elm` file, or continue to the next section:

## Testing from the Command Line
See https://github.com/laszlopandy/elm-console for details, but here's the short version:
Make a file that uses the `Console` runner and sets up the appropriate ports:
```elm
-- Example.elm
import String
import Task

import Console
import ElmTest exposing (..)


tests : Test
tests = 
    suite "A Test Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert False)
        ]


port runner : Signal (Task.Task x ())
port runner =
    Console.run (consoleRunner tests)
```
Then compile it, run the `elm-io.sh` script inside the elm-console directory (you can find this in your project's `elm-stuff`
directory) to process the file, and run it with node:
```bash
$ elm-make Example.elm --output raw-test.js
$ sh ./elm-stuff/packages/laszlopandy/elm-console/1.1.0/elm-io.sh raw-test.js test.js
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
While the `elementRunner` display is nicest to read, the `consoleRunner` runner is amenable to automated testing. If a test
suite passes the script will exit with exit code 0, and if it fails it will exit with 1.

## Integrating With Travis CI

With elm-test and elm-console, it is possible to run continuous integration tests with Travis CI on
your Elm projects. Just set up Travis CI for your repository as normal, write tests with elm-test,
and include a `.travis.yml` file based on the following:
```
language: haskell
install:
  - npm install -g elm
  - elm-package install -y
before_script: 
  - elm-make --yes --output raw-test.js tests/Tests.elm
  - bash elm-stuff/packages/laszlopandy/elm-console/1.0.2/elm-io.sh raw-test.js test.js
script: node test.js
```
You can look at the `.travis.yml` file in this repository to see a real example.

## Running tests with Gulp

You can follow [this gist from turboMaCk](https://gist.github.com/turboMaCk/e2e5bdaee255cd2d1488). Thanks to @turboMaCk for figuring this out.
