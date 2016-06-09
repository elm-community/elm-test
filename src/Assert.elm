module Assert exposing (equal, notEqual, lessThan, greaterThan)

{-| Making assertions.

@docs equal, notEqual, lessThan, greaterThan
-}

import Test exposing (Test)


{-| TODO: docs
-}
equal : { expected : a, actual : a } -> Test
equal { expected, actual } =
    if expected == actual then
        Test.succeed
    else
        Test.fail ("Expected: " ++ toString expected ++ "\nActual:   " ++ toString actual)


{-| TODO: docs
-}
lessThan : { lesser : comparable, greater : comparable } -> Test
lessThan =
    greaterThan


{-| TODO: docs
-}
greaterThan : { lesser : comparable, greater : comparable } -> Test
greaterThan { lesser, greater } =
    if lesser < greater then
        Test.succeed
    else
        Test.fail ("Expected Greater: " ++ toString greater ++ "\nExpected Lesser:  " ++ toString lesser)


{-| TODO docs
-}
notEqual : { actual : a, not : a } -> Test
notEqual record =
    if record.actual == record.not then
        Test.fail ("Expected different values, but both were:\n\n" ++ toString record.actual)
    else
        Test.succeed
