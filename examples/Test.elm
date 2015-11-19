module Test where

import ElmTest exposing (..)


tests : List Test
tests = 
    [ (2^3) `equals` 1
    , 3 `equals` 3
    , defaultTest (assert True)
    , test "test head" (assertEqual 1 (Maybe.withDefault 0 (List.head [1..10])))
    ]


tests2 : List Test
tests2 = 
    [ (2^3) `equals` 8
    , 3 `equals` 3
    , defaultTest (assert True)
    , test "test head" (assertEqual 1 (Maybe.withDefault 0 (List.head [1..10])))
    ]


passingTest : Test
passingTest = 
    test "passing test" (assertEqual 0 0)


failingTest : Test
failingTest = 
    test "failing test" (assertEqual 1 0)


suite1 = 
    suite "Some tests" tests2


suite2 = 
    suite 
        "A Test Suite" 
        [ suite1
        , suite "Some other tests" tests2
        , suite "More tests!" tests2
        , 3 `equals` 3
        , suite "Even more!!" tests2
        ]


suite3 = 
    suite 
        "A Test Suite" 
        [ suite2
        , suite "Some other tests" tests
        , suite "More tests!" tests2
        , 3 `equals` 3
        , suite "Even more!!" tests2
        ]
