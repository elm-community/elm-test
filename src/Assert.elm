module Assert exposing (equal, notEqual, lessThan, greaterThan)

{-| Functions that call `Test.pass` and `Test.fail` with helpful output when
things fail.

@docs equal, notEqual, lessThan, greaterThan
-}

import Test exposing (Outcome)


{-| Fails if `expected /= actual`.
-}
equal : { expected : a, actual : a } -> Outcome
equal { expected, actual } =
    if expected == actual then
        Test.pass
    else
        Test.fail ("Expected: " ++ toString expected ++ "\nActual:   " ++ toString actual)


{-| Fails if `actual == wasNot`.
-}
notEqual : { actual : a, wasNot : a } -> Outcome
notEqual record =
    if record.actual == record.wasNot then
        Test.fail ("Expected different values, but both were:\n\n" ++ toString record.actual)
    else
        Test.pass


{-| Fails if `lesser >= greater`.

(This function is identical to [`greaterThan`](#greaterThan).)
-}
lessThan : { lesser : comparable, greater : comparable } -> Outcome
lessThan =
    greaterThan


{-| Fails if `lesser >= greater`.

(This function is identical to [`lessThan`](#lessThan).)
-}
greaterThan : { lesser : comparable, greater : comparable } -> Outcome
greaterThan { lesser, greater } =
    if lesser < greater then
        Test.pass
    else
        Test.fail ("Expected Greater: " ++ toString greater ++ "\nExpected Lesser:  " ++ toString lesser)



-- TODO should add something like equalLists, equalDicts, and equalSets, which
-- output useful diffs on failure.
