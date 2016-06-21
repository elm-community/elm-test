module Fuzz exposing (Fuzzer, unit, bool, order, array, char, convert, filter, float, floatRange, int, tuple, tuple3, tuple4, tuple5, result, string, percentage, map, maybe, intRange, list, frequency, frequencyOrCrash)

{-| This is a library of `Fuzzer`s you can use to supply values to your fuzz tests.
You can typically pick out which ones you need according to their types.

A `Fuzzer a` knows how to create values of type `a`. It can create them
randomly, and it can shrink them to more minimal values. Fuzzers can be
filtered and mapped over.

## Common Fuzzers
@docs bool, int, intRange, float, floatRange, percentage, string, maybe, result, list, array

## Working with Fuzzers
@docs Fuzzer, filter, convert, map, frequency, frequencyOrCrash

## Tuple Fuzzers
Instead of using a tuple, consider using `fuzzN`.
@docs tuple, tuple3, tuple4, tuple5

## Uncommon Fuzzers
@docs char, unit, order

-}

import Array exposing (Array)
import Char
import Util exposing (..)
import Shrink exposing (Shrinker)
import Random.Pcg as Random exposing (Generator)


{-| A Fuzzer is a
[Random](http://package.elm-lang.org/packages/elm-lang/core/latest/Random)
`Generator` paired with a shrinking strategy, or `Shrinker`. Shrinkers are defined
in [`elm-community/shrink`](http://package.elm-lang.org/packages/elm-community/shrink/latest/).
You will need to be familiar with both libraries to write custom fuzzers for your own types.
Here is an example for a record:

    type alias Position =
        { x : Int, y : Int }


    position : Fuzzer Position
    position =
        Fuzzer
            (Random.map2 Position (Random.int 0 1919) (Random.int 0 1079))
            (\{ x, y } -> Shrink.map Position (Shrink.int x) `Shrink.andMap` (Shrink.int y))

Here is an example for a union type:

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
            Fuzzer generator shrinker
-}
type alias Fuzzer a =
    { generator : Generator a
    , shrinker : Shrinker a
    }


{-| A fuzzer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.
-}
unit : Fuzzer ()
unit =
    Fuzzer (Random.constant ()) Shrink.noShrink


{-| A fuzzer for bool values.
-}
bool : Fuzzer Bool
bool =
    Fuzzer (Random.bool) Shrink.bool


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
        Fuzzer (Random.map intToOrder (Random.int 0 2)) Shrink.order


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
        Fuzzer generator Shrink.int


{-| A fuzzer for int values within between a given minimum and maximum value,
inclusive. Shrunken values will also be within the range.
-}
intRange : Int -> Int -> Fuzzer Int
intRange min max =
    Fuzzer (Random.int min max)
        (Shrink.keepIf (\i -> i >= min && i <= max) Shrink.int)


{-| A fuzzer for float values. It will never fuzzuce `NaN`, `Infinity`, or `-Infinity`.
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
        Fuzzer generator Shrink.float


{-| A fuzzer for float values within between a given minimum and maximum
value, inclusive. Shrunken values will also be within the range.
-}
floatRange : Float -> Float -> Fuzzer Float
floatRange min max =
    Fuzzer (Random.float min max)
        (Shrink.keepIf (\i -> i >= min && i <= max) Shrink.float)


{-| A fuzzer for percentage values. Generates random floats between `0.0` and
`1.0`.
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
        Fuzzer generator Shrink.float


{-| A fuzzer for char values. Generates random ascii chars disregarding the control
characters.
-}
char : Fuzzer Char
char =
    Fuzzer (Random.map Char.fromCode (Random.int 32 126)) Shrink.character


{-| A fuzzer for string values. Generates random printable ascii strings whose
length is between 0 and 10.
-}
string : Fuzzer String
string =
    Fuzzer (rangeLengthString 0 10 char.generator)
        Shrink.string


{-| Given a fuzzer of a type, create a fuzzer of a maybe for that type.
-}
maybe : Fuzzer a -> Fuzzer (Maybe a)
maybe fuzz =
    let
        genBool =
            Random.map not <| Random.oneIn 4
    in
        Fuzzer (Random.maybe genBool fuzz.generator) (Shrink.maybe fuzz.shrinker)


{-| Given fuzzers for an error type and a success type, create a fuzzer for
a result.
-}
result : Fuzzer error -> Fuzzer value -> Fuzzer (Result error value)
result errFuzz valFuzz =
    Fuzzer
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
list fuzz =
    Fuzzer
        (Random.frequency
            [ ( 1, Random.constant [] )
            , ( 1, Random.map (\x -> [ x ]) fuzz.generator )
            , ( 3, rangeLengthList 2 10 fuzz.generator )
            , ( 2, rangeLengthList 10 100 fuzz.generator )
            , ( 0.5, rangeLengthList 100 400 fuzz.generator )
            ]
        )
        (Shrink.list fuzz.shrinker)


{-| Given a fuzzer of a type, create a fuzzer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.
-}
array : Fuzzer a -> Fuzzer (Array a)
array fuzz =
    Fuzzer
        (Random.frequency
            [ ( 1, Random.constant Array.empty )
            , ( 1, Random.map (Array.repeat 1) fuzz.generator )
            , ( 3, rangeLengthArray 2 10 fuzz.generator )
            , ( 2, rangeLengthArray 10 100 fuzz.generator )
            , ( 0.5, rangeLengthArray 100 400 fuzz.generator )
            ]
        )
        (Shrink.array fuzz.shrinker)


{-| Turn a tuple of fuzzers into a fuzzer of tuples.
-}
tuple : ( Fuzzer a, Fuzzer b ) -> Fuzzer ( a, b )
tuple ( fuzzA, fuzzB ) =
    Fuzzer (Random.map2 (,) fuzzA.generator fuzzB.generator)
        (Shrink.tuple ( fuzzA.shrinker, fuzzB.shrinker ))


{-| Turn a 3-tuple of fuzzers into a fuzzer of 3-tuples.
-}
tuple3 : ( Fuzzer a, Fuzzer b, Fuzzer c ) -> Fuzzer ( a, b, c )
tuple3 ( fuzzA, fuzzB, fuzzC ) =
    Fuzzer (Random.map3 (,,) fuzzA.generator fuzzB.generator fuzzC.generator)
        (Shrink.tuple3 ( fuzzA.shrinker, fuzzB.shrinker, fuzzC.shrinker ))


{-| Turn a 4-tuple of fuzzers into a fuzzer of 4-tuples.
-}
tuple4 : ( Fuzzer a, Fuzzer b, Fuzzer c, Fuzzer d ) -> Fuzzer ( a, b, c, d )
tuple4 ( fuzzA, fuzzB, fuzzC, fuzzD ) =
    Fuzzer (Random.map4 (,,,) fuzzA.generator fuzzB.generator fuzzC.generator fuzzD.generator)
        (Shrink.tuple4 ( fuzzA.shrinker, fuzzB.shrinker, fuzzC.shrinker, fuzzD.shrinker ))


{-| Turn a 5-tuple of fuzzers into a fuzzer of 5-tuples.
-}
tuple5 : ( Fuzzer a, Fuzzer b, Fuzzer c, Fuzzer d, Fuzzer e ) -> Fuzzer ( a, b, c, d, e )
tuple5 ( fuzzA, fuzzB, fuzzC, fuzzD, fuzzE ) =
    Fuzzer (Random.map5 (,,,,) fuzzA.generator fuzzB.generator fuzzC.generator fuzzD.generator fuzzE.generator)
        (Shrink.tuple5 ( fuzzA.shrinker, fuzzB.shrinker, fuzzC.shrinker, fuzzD.shrinker, fuzzE.shrinker ))


{-| Filter the values from a fuzzer. The resulting Fuzzer will only generate
random test values or shrunken values that satisfy the predicate. The predicate
must be satisfiable.
-}
filter : (a -> Bool) -> Fuzzer a -> Fuzzer a
filter predicate fuzz =
    Fuzzer (Random.filter predicate fuzz.generator)
        (Shrink.keepIf predicate fuzz.shrinker)


{-| Convert the output of one fuzzer to another type. This is useful if
you're testing a function that expects a large model record, but you only need
to randomize a few fields. You might do this several different ways for a single
model, so you generate and shrink only the fields relevant to each test.

    type alias Person =
      { first : String, last : String, age : String }

    spy : Fuzzer Person
    spy = convert (\age -> Person "James" "Bond" age) .age (intRange 0 120)

In order for shrinking to work, you need to pass an inverse function of the
function being mapped.
-}
convert : (a -> b) -> (b -> a) -> Fuzzer a -> Fuzzer b
convert f g fuzz =
    Fuzzer (Random.map f fuzz.generator)
        (Shrink.convert f g fuzz.shrinker)


{-| Map a function over a fuzzer. This works exactly like `convert`,
except it does not require an inverse function, and consequently does no
shrinking.
-}
map : (a -> b) -> Fuzzer a -> Fuzzer b
map f fuzz =
    Fuzzer (Random.map f fuzz.generator)
        Shrink.noShrink


{-| Create a new `Fuzzer` by providing a list of probabilistic weights to use
with other fuzzers.

For example, to create a `Fuzzer` that has a 1/4 chance of generating an int
between -1 and -100, and a 3/4 chance of generating one between 1 and 100,
you could do this:

    Fuzz.freuqency
        [ (1, Fuzz.intRange (-100, -1))
        , (3, Fuzz.intRange (1, 100))
        ]

This returns a `Result` because it can fail in a few ways:

* If you provide an empy list of frequencies
* If any of the weights are less than 0
* If the weights sum to 0

Any of these will lead to a result of `Err`, with a `String` explaining what
went wrong.
-}
frequency : List ( Float, Fuzzer a ) -> Result String (Fuzzer a)
frequency list =
    case List.head list of
        Nothing ->
            Err "You must provide at least one frequency pair."

        Just ( _, { shrinker } ) ->
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
                    Ok (Fuzzer generator shrinker)


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
toGeneratorFrequency ( weight, { generator } ) =
    ( weight, generator )
