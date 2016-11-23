# elm-test [![Travis build Status](https://travis-ci.org/elm-community/elm-test.svg?branch=master)](http://travis-ci.org/elm-community/elm-test)

Write unit and fuzz tests for your Elm code, in Elm.

## Quick Start

Here are three example tests:

```elm
suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse" -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \() ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \() ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
```

This code uses a few common functions:

* [`describe`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Test#test) to add a description string to a list of tests
* [`test`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Test#test) to write a unit test
* [`Expect`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Expect) to determine if a test should pass or fail
* [`fuzz`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Test#fuzz) to run a function that produces a test several times with randomly-generated inputs

Check out [a large real-world test suite](https://github.com/rtfeldman/elm-css/tree/master/tests) for more.

### Running tests locally

There are several ways you can run tests locally:

* [from your terminal](https://github.com/rtfeldman/node-test-runner) via `npm install -g elm-test`
* [from your browser](https://github.com/rtfeldman/html-test-runner)

Here's how to set up and run your tests using the CLI test runner:

1. Run `npm install -g elm-test` if you haven't already.
2. `cd` into the project's root directory that has your `elm-package.json`.
3. Run `elm-test init`. It will create a `tests` directory inside this one,
   with some files in it.
4. Copy all the dependencies from `elm-package.json` into
   `tests/elm-package.json`. These dependencies need to stay in sync, so make
   sure whenever you change your dependencies in your current
   `elm-package.json`, you make the same change to `tests/elm-package.json`.
5. Run `elm-test`.
6. Edit `tests/Tests.elm` to introduce new tests.

### Running tests on CI

Here are some examples of running tests on CI servers:

* [`travis.yml`](https://github.com/rtfeldman/elm-css/blob/6ba8404f53269bc110c2e08ab24c9caf850da515/.travis.yml)
* [`appveyor.yml`](https://github.com/rtfeldman/elm-css/blob/6ba8404f53269bc110c2e08ab24c9caf850da515/appveyor.yml)

## Strategies for effective testing

* [Make impossible states unrepresentable](https://www.youtube.com/watch?v=IcgmSRJHu_8) so that you don't have to test that they can't occur.
* When doing TDD, treat compiler errors as a red test. So feel free to write the test you wish you had even if it means calling functions that don't exist yet!
* How do you know when to stop testing? This is an engineering tradeoff without a perfect answer. If you don't feel confident in the correctness of your code, write more tests. If you feel you are wasting time testing better spent writing your application, stop writing tests for now.
* Prefer fuzz tests to unit tests, when possible. But don't be afraid to supplement them with unit tests for tricky cases and regressions.
* For simple functions, it's okay to copy the implementation to the test; this is a useful regression check. But if the implementation isn't obviously right, try to write tests that don't duplicate the suspect logic. The great thing about fuzz tests is that you don't have to arrive at the exact same value as the code under test, just state something that will be true of that value.
* If you're writing a library that wraps an existing standard or protocol, use examples from the specification or docs as unit tests. Anything in your README should be backed by a unit test (sadly there's no easy way to keep them in sync).
* Not even your test modules can import unexposed functions, so test them only as the exposed interface uses them. Don't expose a function just to test it. Every exposed function should have tests. (If you practice TDD, this happens automatically!)
* `elm-test` is designed to test functions, not effects. To test them, use [elm-testable](http://package.elm-lang.org/packages/avh4/elm-testable/latest). For integration or end-to-end testing, use your favorite PhantomJS or Selenium webdriver, such as Capybara.

## Upgrading
### From 0.17
You will need to delete `elm-stuff` and `tests/elm-stuff`.

If you are using the Node runner, you will need to pull down the new `Main.elm`: `curl -o tests/Main.elm https://raw.githubusercontent.com/rtfeldman/node-test-runner/master/templates/Main.elm`

### From the old elm-test
[`legacy-elm-test`](http://package.elm-lang.org/packages/rtfeldman/legacy-elm-test/latest) provides a
drop-in replacement for the `ElmTest 1.0` API, except implemented in terms of
the current `elm-test`. It also includes support for `elm-check` tests.

This lets you use the latest test runners right now, and upgrade incrementally.

## Releases
| Version | Notes |
| ------- | ----- |
| [**3.1.0**](https://github.com/elm-community/elm-test/tree/3.1.0) | Add Expect.all
| [**3.0.0**](https://github.com/elm-community/elm-test/tree/3.0.0) | Update for Elm 0.18; switch the argument order of `Fuzz.andMap`.
| [**2.1.0**](https://github.com/elm-community/elm-test/tree/2.1.0) | Switch to rose trees for `Fuzz.andThen`, other API additions.
| [**2.0.0**](https://github.com/elm-community/elm-test/tree/2.0.0) | Scratch-rewrite to project-fuzzball
| [**1.0.0**](https://github.com/elm-community/elm-test/tree/1.0.0) | ElmTest initial release
