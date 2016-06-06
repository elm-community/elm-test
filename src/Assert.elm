module Assert exposing (Outcome(Success, Failure), equal, Assertion, assert, assertFuzz, resolve)

import Random.Pcg as Random


{-| The outcome from running a single test.
-}
type Outcome
    = Success
    | Failure (List String)


{-| TODO: docs
-}
type Assertion
    = Assertion (Random.Seed -> Int -> Bool -> Outcome)


{-| TODO: docs
-}
equal : { expected : a, actual : a } -> Assertion
equal { expected, actual } =
    let
        run _ _ _ =
            if expected == actual then
                Success
            else
                Failure [ "Expected: " ++ toString expected, "Actual:   " ++ toString actual ]
    in
        Assertion run


assert : Outcome -> Assertion
assert outcome =
    Assertion (\_ _ _ -> outcome)


assertFuzz : (Random.Seed -> Int -> Bool -> Outcome) -> Assertion
assertFuzz =
    Assertion


resolve : Random.Seed -> Int -> Bool -> Assertion -> Outcome
resolve seed runs doShrink (Assertion run) =
    run seed runs doShrink
