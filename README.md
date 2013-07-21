Elm-Test
========

A unit testing framework for Elm

## Creating Tests

Creating a test case is very simple. You only need a name and an assertion:
```
myTest = test "Example Test" (assert True)
```
For convenience, there is a function to create a name for you based on the inputs:
```
-- Test name will be "5 == 5"
myTest = defaultTest (assertEqual 5 5)
```
There are different types of assertions:
```
assert
assertEqual
assertNotEqual
```
As well as a couple operators for quickly creating assertions and tests (inspired by HUnit):
```
a @=? b = assertEqual a b
a @/=? b = assertNotEqual a b
a ~=? b = defaultTest <| assertEqual a b
```
And finally, a test generator. It requires a name (otherwise things could get too confusing!), a function to test, a list of tuples. The first item in the tuple is the function input, and the second item in the tuple is the expected output. It will generate a list of tests for you:
```
myTests = testFunction "Square" (\x -> x^2) [ (1, 1)
                                            , (2, 4)
                                            , (3, 9)
                                            , (4, 16)
                                            , (5, 25)
                                            ]
```
Since ```testFunction``` only accepts unary functions, there is an additional convenience function ```testFunction2``` provided to generate tests for binary functions. This way you don't need to zip your arguments together and uncurry your function, as long as it only has 2 arguments:
```
myTests = testFunction2 "Add" (+) [ ((1, 1), 2)
                                  , ((2, 2), 4)
                                  , ((10, 0), 10)
                                  ]
```

## Running Tests

Running a test produces a result. A result is either a pass or a failure, so it is represented as ```Either String String```. 

The most basic way to run a test is the ```run``` function, which has the type signature ```Test -> Result```

A list of tests can be run all at once. Given a list of tests ```myTests```, you can write:
```
runTests myTests
```
This is all well and good, but there's another way to quickly visualize the results: the ```runPrettyTests``` function. This has the type signature ```[Test] -> Element``` so it's pretty easy to use. It will display total tests run, the number passed, the number failed, and then 2 columns. The left column has the test names, while the right column has their results, colored green for a passing test or red (with accompanying error message) for a failing test. This can be easily put in the ```main``` function of an Elm module:
```
main = runPrettyTests myTests
```

## Demo

For a quick demo, you can copy and paste the entire contents of the ElmTest.elm file into [elm-lang.org's online editor](http://elm-lang.org/try/), or check it out on [Share-Elm](http://share-elm.com/sprout/51eb5eb5e4b07e418296d0af)
