# elm-test

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

This code includes a few common things:

* [`describe`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Test#test) to add a description string to a list of tests
* [`test`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Test#test) to write a unit test
* [`Expect`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Expect) to determine if a test should pass or fail
* [`fuzz`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Test#fuzz) to run a function that produces a test several times with randomly-generated inputs

Check out [a more complete example](https://github.com/elm-community/elm-test/tree/master/examples) or [a large real-world test suite](https://github.com/rtfeldman/elm-css/tree/master/test) for more.

### Running tests locally

There are several ways you can run tests locally:

* [from your terminal](https://github.com/rtfeldman/node-test-runner) via `npm install -g elm-test`
* [from your browser](https://github.com/elm-community/elm-test-runner)
* [using a custom runner](https://github.com/elm-community/elm-test/blob/master/example/LogRunnerExample.elm) of your own design

### Running tests on CI

Here are some examples of running tests on CI servers:

* [`travis.yml`](https://github.com/rtfeldman/elm-css/blob/6ba8404f53269bc110c2e08ab24c9caf850da515/.travis.yml)
* [`appveyor.yml`](https://github.com/rtfeldman/elm-css/blob/6ba8404f53269bc110c2e08ab24c9caf850da515/appveyor.yml)
