elm-test [![Build Status](https://travis-ci.org/elm-community/elm-test.png?branch=master)](https://travis-ci.org/elm-community/elm-test)

Write tests in Elm!

## Sample Code

```elm
test : Test
test =
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

## Running tests locally

There are several ways to run tests locally:

* [from your terminal](https://github.com/rtfeldman/node-test-runner) via `npm install -g elm-test`
* [from your browser](https://github.com/elm-community/elm-test-runner)
* [using a custom runner](https://github.com/elm-community/elm-test/blob/master/example/LogRunnerExample.elm) of your own design

## Running tests on CI

Here are some examples of running tests on CI servers:

* [`travis.yml`](https://github.com/rtfeldman/elm-css/blob/6ba8404f53269bc110c2e08ab24c9caf850da515/.travis.yml)
* [`appveyor.yml`](https://github.com/rtfeldman/elm-css/blob/6ba8404f53269bc110c2e08ab24c9caf850da515/appveyor.yml)
