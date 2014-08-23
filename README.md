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
As well as a function to create an assertEqual tests, again deriving a name based on the inputs:
```haskell
myTest = 5 `equals` 5
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

## Grouping Tests

Writing many tests as a flat list quickly becomes unwieldy. For easier maintenance you can group tests into logical
units called test suites. The following function will create a test suite from a suite name and a list of tests:
```haskell
suite : String -> [Test] -> Test
```
The type of a test suite is simply `Test`, allowing use of all the test runners with either a single test or a suite of tests. Test suites can also contain subsuites, of course.

The other benefit of grouping tests into suites is that the test runners described in the following sections will greatly simplify the output, showing only detailed information in suites that contain failed tests, making it easier to quickly spot the failures instead of being flooded with irrelevant data.

## Running Tests

Running a test produces a result. A result is either a pass, a failure, or a report containing detailed results from
the tests or subsuites contained in a suite. All results contain the name of the test or suite that was run, and failures additionally contain a failure message giving a hint as to why the test failed.

The most basic way to run a test is the `run` function, which has the type signature `Test -> Result`. A test suite can also be run all at once, again with the `run` function.

A `Report` is of type `{results : [Result], passes : [Result], failures : [Result]}`.
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
Compile this with `elm --make Example.elm` and open the resulting `build/Example.html` file in your browser, and you'll see the results.

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

This library is also designed for interoperability with the nodejs-based Elm IO library from Max New. This interoperability is still somewhat experimental, so to make things easier, I recommend building the IO library [from the 0.1 tag](https://github.com/maxsnew/IO/tree/0.1). Once that's done, there's still a bit of setup to be done before you can continue. From the root directory of this repository, run:
```bash
$ elm-get install evancz/automaton
Cloning repo evancz/automaton
Checking out version 0.1.0.1
Should I add this library to your elm_dependencies.json file? (y/n): y
Success!
```
Continue with:
```bash
$ npm install jsdom
```
(On Windows, `jsdom` is somewhat difficult to install. [Refer to this blog post](http://www.steveworkman.com/node-js/2012/installing-jsdom-on-windows/) for detailed instructions)

And now you're ready to actually run the example:
```bash
$ elm-io ScriptExample.elm ScriptExample.js
[2 of 10] Compiling ElmTest.Assertion   ( ElmTest/Assertion.elm )
[3 of 10] Compiling ElmTest.Test        ( ElmTest/Test.elm )
[4 of 10] Compiling ElmTest.Run         ( ElmTest/Run.elm )
[5 of 10] Compiling ElmTest.Runner.String ( ElmTest/Runner/String.elm )
[6 of 10] Compiling ElmTest.Runner.Console ( ElmTest/Runner/Console.elm )
[7 of 10] Compiling Test                ( Test.elm )
[8 of 10] Compiling Automaton           ( elm_dependencies/evancz-automaton/0.1.0.1/Automaton.elm )
[9 of 10] Compiling IO.Runner           ( .../cabal/ElmIO-0.1.0.0/IO/Runner.elm )
[10 of 10] Compiling Main                ( ScriptExample.elm )
Generating JavaScript ... Done
Making exe

$ node ScriptExample.js
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
And that's it! Once `elm-io` is set up like this, you can run tests on the command line from any directory, in any Elm project. Just make sure to pull in the `evancz/automaton` and `jsdom` dependencies. Two current restrictions are that the module name of the file you wish to compile must be `Main`, and the following boilerplate must be added to this `Main` module:
```haskell
port requests : Signal Request
port requests = Run.run responses console -- <Replace console with name of the function of type IO () which runs your tests>

port responses : Signal Response
```
You can examine `ScriptExample.elm` to see exactly how these are required. For a detailed log capturing the entire
setup process for the command line example, see: [ScriptExample.md](https://github.com/deadfoxygrandpa/Elm-Test/blob/master/ScriptExample.md).

Since `elm-io` version 0.1, it's possible to ignore most of this boilerplate in favor of defining a
`console : IO ()` function, which effectively replaces the `main : Signal Element` function in normal
graphical Elm programs. Compile with `elm-io --default-ports Tests.elm tests.js`, replacing `Tests.elm`
with the filename of your Elm source file, and `tests.js` with the desired output script name to be
run with node. With this `--default-ports` flag, a valid console-run test file is:
```haskell
module Main where

import ElmTest.Runner.Console (runDisplay)
import open ElmTest.Test

tests : Test
tests = suite "A Test Suite"
        [ 5 `equals` 5
        , test "Addition" (assertEqual (3 + 7) 10)
        ]

console = runDisplay tests
```
That's it! Make sure `evancz/automaton` and `jsdom` are installed in the project directory, then
compile like `elm-io --default-ports Tests.elm tests.js`. Run `node tests.js` and you will get:
```
1 suites run, containing  2 tests
All tests passed

```
with exit code 0. If any tests fail, the process will exit with exit code 1.

## Integrating With Travis CI

With Elm-Test and Elm IO, it is now possible to run continuous integration tests with Travis CI on
your Elm projects. Just set up Travis CI for your repository as normal, write tests with Elm-Test,
and include a `.travis.yml` file based on this template:
```
language: haskell
install:
  - cabal install elm-get
  - sudo ln -s ~/.cabal/bin/elm /usr/local/bin/elm
  - sudo ln -s ~/.cabal/bin/elm-get /usr/local/bin/elm-get
  - git clone git://github.com/maxsnew/IO
  - cd IO
  - git checkout tags/0.1
  - cabal install
  - sudo ln -s ~/.cabal/bin/elm-io /usr/local/bin/elm-io
  - cd ..
  - echo "y" | elm-get install evancz/automaton
  - echo "y" | elm-get install deadfoxygrandpa/Elm-Test
  - npm install jsdom
before_script: elm-io --default-ports Tests/Tests.elm tests.js
script: node tests.js
```
If your tests are in `Tests/Tests.elm` then this `.travis.yml` file will work out o the box, with no
changes necessary. 

This repository itself is using almost this exact Travis CI configuration as a proof of concept. It doesn't
make a lot of sense to do extensive testing of the library using itself, but the exact same techniques 
can be applied to any Elm project.
