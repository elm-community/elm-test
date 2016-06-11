module Test exposing (Test, Outcome, Suite)

{-| Testing

@docs Test, Outcome, Suite, pass, fail, it, failWith, toFailures, formatFailures, suite
-}

import Random.Pcg as Random
import Test.Suite
import Test.Outcome


type alias Test =
    Test.Outcome.Test


type alias Suite =
    Test.Suite.Suite
