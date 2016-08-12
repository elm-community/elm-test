module Fuzz exposing (Fuzzer, custom, unit, bool, order, array, char, float, floatRange, int, tuple, tuple3, tuple4, tuple5, result, string, percentage, map, maybe, intRange, list, frequency, frequencyOrCrash)

{-| This is a library of *fuzzers* you can use to supply values to your fuzz
tests. You can typically pick out which ones you need according to their types.

A `Fuzzer a` knows how to create values of type `a` in two different ways. It
can create them randomly, so that your test's expectations are run against many
values. Fuzzers will often generate edge cases likely to find bugs. If the
fuzzer can make your test fail, it also knows how to "shrink" that failing input
into more minimal examples, some of which might also cause the tests to fail. In
this way, fuzzers can usually find the smallest or simplest input that
reproduces a bug.

## Common Fuzzers
@docs bool, int, intRange, float, floatRange, percentage, string, maybe, result, list, array

## Working with Fuzzers
@docs Fuzzer, map, frequency, frequencyOrCrash

## Tuple Fuzzers
Instead of using a tuple, consider using `fuzzN`.
@docs tuple, tuple3, tuple4, tuple5

## Uncommon Fuzzers
@docs custom, char, unit, order

-}

import Array exposing (Array)
import Char
import Util exposing (..)
import Shrink exposing (Shrinker)
import Random.Pcg as Random exposing (Generator)
import Fuzz.Internal as Internal


{-| The representation of fuzzers is opaque. Conceptually, a `Fuzzer a`
consists of a way to randomly generate values of type `a`, and a way to shrink
those values.
-}
type alias Fuzzer a =
    Internal.Fuzzer a


{-| Build a custom `Fuzzer a` by providing a `Generator a` and a `Shrinker a`.
Generators are defined by [`mgold/elm-random-pcg`](http://package.elm-lang.org/packages/mgold/elm-random-pcg/latest),
which is not core's Random module but has a compatible interface. Shrinkers are
defined in [`elm-community/shrink`](http://package.elm-lang.org/packages/elm-community/shrink/latest/).

Here is an example for a record:

    import Random.Pcg as Random
    import Shrink

    type alias Position =
        { x : Int, y : Int }


    position : Fuzzer Position
    position =
        Fuzz.custom
            (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
            (\{ x, y } -> Shrink.map Position (Shrink.int x) `Shrink.andMap` (Shrink.int y))

Here is an example for a custom union type:

    type Question
        = Name String
        | Age Int


    question =
        let
            generator =
                Random.bool `Random.andThen` (\b ->
                    if b then
                        Random.map Name string.generator
                    else
                        Random.map Age (Random.int 0 120)
                 )

            shrinker question =
                case question of
                    Name n ->
                        Shrink.string n |> Shrink.map Name

                    Age i ->
                        Shrink.int i |> Shrink.map Age
        in
            Fuzz.custom generator shrinker
-}
custom : Generator a -> Shrinker a -> Fuzzer a
custom generator shrinker =
    Internal.Fuzzer { generator = generator, shrinker = shrinker }


{-| A fuzzer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.
-}
unit : Fuzzer ()
unit =
    custom (Random.constant ()) Shrink.noShrink


{-| A fuzzer for bool values.
-}
bool : Fuzzer Bool
bool =
    custom (Random.bool) Shrink.bool


{-| A fuzzer for order values.
-}
order : Fuzzer Order
order =
    let
        intToOrder i =
            if i == 0 then
                LT
            else if i == 1 then
                EQ
            else
                GT
    in
        custom (Random.map intToOrder (Random.int 0 2)) Shrink.order


{-| A fuzzer for int values.
-}
int : Fuzzer Int
int =
    let
        generator =
            Random.frequency
                [ ( 3, Random.int -50 50 )
                , ( 0.2, Random.constant 0 )
                , ( 1, Random.int 0 (Random.maxInt - Random.minInt) )
                , ( 1, Random.int (Random.minInt - Random.maxInt) 0 )
                ]
    in
        custom generator Shrink.int


{-| A fuzzer for int values within between a given minimum and maximum value,
inclusive. Shrunken values will also be within the range.
-}
intRange : Int -> Int -> Fuzzer Int
intRange min max =
    custom (Random.int min max)
        (Shrink.keepIf (\i -> i >= min && i <= max) Shrink.int)


{-| A fuzzer for float values. It will never produce `NaN`, `Infinity`, or `-Infinity`.
-}
float : Fuzzer Float
float =
    let
        generator =
            Random.frequency
                [ ( 3, Random.float -50 50 )
                , ( 0.5, Random.constant 0 )
                , ( 1, Random.float -1 1 )
                , ( 1, Random.float 0 (toFloat <| Random.maxInt - Random.minInt) )
                , ( 1, Random.float (toFloat <| Random.minInt - Random.maxInt) 0 )
                ]
    in
        custom generator Shrink.float


{-| A fuzzer for float values within between a given minimum and maximum
value, inclusive. Shrunken values will also be within the range.
-}
floatRange : Float -> Float -> Fuzzer Float
floatRange min max =
    custom (Random.float min max)
        (Shrink.keepIf (\i -> i >= min && i <= max) Shrink.float)


{-| A fuzzer for percentage values. Generates random floats between `0.0` and
`1.0`. It will test zero and one about 10% of the time each.
-}
percentage : Fuzzer Float
percentage =
    let
        generator =
            Random.frequency
                [ ( 8, Random.float 0 1 )
                , ( 1, Random.constant 0 )
                , ( 1, Random.constant 1 )
                ]
    in
        custom generator Shrink.float


{-| A fuzzer for char values. Generates random ascii chars disregarding the control
characters.
-}
char : Fuzzer Char
char =
    custom charGenerator Shrink.character


charGenerator : Generator Char
charGenerator =
    (Random.map Char.fromCode (Random.int 32 126))


{-| A fuzzer for string values. Generates random printable ascii strings whose
length is between 0 and 10.
-}
string : Fuzzer String
string =
    custom (rangeLengthString 0 10 charGenerator)
        Shrink.string


{-| Given a fuzzer of a type, create a fuzzer of a maybe for that type.
-}
maybe : Fuzzer a -> Fuzzer (Maybe a)
maybe (Internal.Fuzzer { generator, shrinker }) =
    let
        genBool =
            Random.map not <| Random.oneIn 4
    in
        custom (Random.maybe genBool generator) (Shrink.maybe shrinker)


{-| Given fuzzers for an error type and a success type, create a fuzzer for
a result.
-}
result : Fuzzer error -> Fuzzer value -> Fuzzer (Result error value)
result (Internal.Fuzzer errFuzz) (Internal.Fuzzer valFuzz) =
    custom
        (Random.bool
            `Random.andThen`
                (\b ->
                    if b then
                        Random.map Err errFuzz.generator
                    else
                        Random.map Ok valFuzz.generator
                )
        )
        (Shrink.result errFuzz.shrinker valFuzz.shrinker)


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of varying length, favoring shorter lists.
-}
list : Fuzzer a -> Fuzzer (List a)
list (Internal.Fuzzer { generator, shrinker }) =
    custom
        (Random.frequency
            [ ( 1, Random.constant [] )
            , ( 1, Random.map (\x -> [ x ]) generator )
            , ( 3, rangeLengthList 2 10 generator )
            , ( 2, rangeLengthList 10 100 generator )
            , ( 0.5, rangeLengthList 100 400 generator )
            ]
        )
        (Shrink.list shrinker)


{-| Given a fuzzer of a type, create a fuzzer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.
-}
array : Fuzzer a -> Fuzzer (Array a)
array (Internal.Fuzzer { generator, shrinker }) =
    custom
        (Random.frequency
            [ ( 1, Random.constant Array.empty )
            , ( 1, Random.map (Array.repeat 1) generator )
            , ( 3, rangeLengthArray 2 10 generator )
            , ( 2, rangeLengthArray 10 100 generator )
            , ( 0.5, rangeLengthArray 100 400 generator )
            ]
        )
        (Shrink.array shrinker)


{-| Turn a tuple of fuzzers into a fuzzer of tuples.
-}
tuple : ( Fuzzer a, Fuzzer b ) -> Fuzzer ( a, b )
tuple ( Internal.Fuzzer fuzzA, Internal.Fuzzer fuzzB ) =
    custom (Random.map2 (,) fuzzA.generator fuzzB.generator)
        (Shrink.tuple ( fuzzA.shrinker, fuzzB.shrinker ))


{-| Turn a 3-tuple of fuzzers into a fuzzer of 3-tuples.
-}
tuple3 : ( Fuzzer a, Fuzzer b, Fuzzer c ) -> Fuzzer ( a, b, c )
tuple3 ( Internal.Fuzzer fuzzA, Internal.Fuzzer fuzzB, Internal.Fuzzer fuzzC ) =
    custom (Random.map3 (,,) fuzzA.generator fuzzB.generator fuzzC.generator)
        (Shrink.tuple3 ( fuzzA.shrinker, fuzzB.shrinker, fuzzC.shrinker ))


{-| Turn a 4-tuple of fuzzers into a fuzzer of 4-tuples.
-}
tuple4 : ( Fuzzer a, Fuzzer b, Fuzzer c, Fuzzer d ) -> Fuzzer ( a, b, c, d )
tuple4 ( Internal.Fuzzer fuzzA, Internal.Fuzzer fuzzB, Internal.Fuzzer fuzzC, Internal.Fuzzer fuzzD ) =
    custom (Random.map4 (,,,) fuzzA.generator fuzzB.generator fuzzC.generator fuzzD.generator)
        (Shrink.tuple4 ( fuzzA.shrinker, fuzzB.shrinker, fuzzC.shrinker, fuzzD.shrinker ))


{-| Turn a 5-tuple of fuzzers into a fuzzer of 5-tuples.
-}
tuple5 : ( Fuzzer a, Fuzzer b, Fuzzer c, Fuzzer d, Fuzzer e ) -> Fuzzer ( a, b, c, d, e )
tuple5 ( Internal.Fuzzer fuzzA, Internal.Fuzzer fuzzB, Internal.Fuzzer fuzzC, Internal.Fuzzer fuzzD, Internal.Fuzzer fuzzE ) =
    custom (Random.map5 (,,,,) fuzzA.generator fuzzB.generator fuzzC.generator fuzzD.generator fuzzE.generator)
        (Shrink.tuple5 ( fuzzA.shrinker, fuzzB.shrinker, fuzzC.shrinker, fuzzD.shrinker, fuzzE.shrinker ))


{-| Map a function over a fuzzer. Due to technical limitations, the resulting
fuzzer performs no shrinking.
-}
map : (a -> b) -> Fuzzer a -> Fuzzer b
map f (Internal.Fuzzer { generator }) =
    custom (Random.map f generator)
        Shrink.noShrink


{-| Create a new fuzzer by providing a list of fuzzers to pick from. Each fuzzer
is associated with a `Float` weight; larger numbers mean the fuzzer in more
likely to get picked from.

For example, to create a `Fuzzer` that has a 1/4 chance of generating an int
between -1 and -100, and a 3/4 chance of generating one between 1 and 100,
you could do this:

    Fuzz.frequency
        [ ( 1, Fuzz.intRange -100 -1 )
        , ( 3, Fuzz.intRange 1 100 )
        ]

This function returns a `Result` because it can fail in a few ways:

* If you provide an empy list of frequencies
* If any of the weights are less than 0
* If the weights sum to 0

Any of these will lead to an `Err`, with a `String` explaining what went wrong.
-}
frequency : List ( Float, Fuzzer a ) -> Result String (Fuzzer a)
frequency list =
    case List.head list of
        Nothing ->
            Err "You must provide at least one frequency pair."

        Just ( _, Internal.Fuzzer { shrinker } ) ->
            if List.any (\( weight, _ ) -> weight < 0) list then
                Err "No frequency weights can be less than 0."
            else if List.sum (List.map fst list) <= 0 then
                Err "Frequency weights must sum to more than 0."
            else
                let
                    generator =
                        list
                            |> List.map toGeneratorFrequency
                            |> Random.frequency
                in
                    Ok (custom generator shrinker)


{-| Calls `frequency` and handles `Err` results by crashing with the given
error message.

This is useful in tests, where a crash will simply cause the test run to fail.
There is no danger to a production system there.
-}
frequencyOrCrash : List ( Float, Fuzzer a ) -> Fuzzer a
frequencyOrCrash =
    frequency >> okOrCrash


okOrCrash : Result String a -> a
okOrCrash result =
    case result of
        Ok a ->
            a

        Err str ->
            Debug.crash str


toGeneratorFrequency : ( Float, Fuzzer a ) -> ( Float, Generator a )
toGeneratorFrequency ( weight, Internal.Fuzzer { generator } ) =
    ( weight, generator )
