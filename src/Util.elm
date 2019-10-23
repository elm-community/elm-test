module Util exposing (..)

{-| This is where I'm sticking Random helper functions I don't want to add to Pcg.
-}

import Array exposing (Array)
import Random.Pcg exposing (..)
import String


rangeLengthList : Int -> Int -> Generator a -> Generator (List a)
rangeLengthList minLength maxLength generator =
    int minLength maxLength
        |> andThen (\len -> list len generator)


rangeLengthArray : Int -> Int -> Generator a -> Generator (Array a)
rangeLengthArray minLength maxLength generator =
    rangeLengthList minLength maxLength generator
        |> map Array.fromList


rangeLengthString : Int -> Int -> Generator Char -> Generator String
rangeLengthString minLength maxLength charGenerator =
    int minLength maxLength
        |> andThen (lengthString charGenerator)


lengthString : Generator Char -> Int -> Generator String
lengthString charGenerator stringLength =
    list stringLength charGenerator
        |> map String.fromList


{-| Creates a generator from either a single element from a single generator,
a single one of the generators, a pair of the generators or all of the generators,
then runs Fuzz.frequency on that subset until we have the desired length list.

**Warning:** Do not pass an empty list or your program will crash! In practice
this is usually not a problem since you pass a list literal.

-}
frequencyList : Generator Int -> List ( Float, Generator a ) -> Generator (List a)
frequencyList lengthGenerator pairs =
    let
        weightedPairs =
            pairs |> List.map (\( a, b ) -> ( a, constant ( a, b ) ))

        randomGenerator : Generator ( Float, Generator a )
        randomGenerator =
            weightedPairs |> frequency

        generator : Generator (Generator a)
        generator =
            sample
                [ -- single repeated element for a single generator
                  pairs
                    |> frequency
                    |> map constant

                -- single generator
                , randomGenerator
                    |> map List.singleton
                    |> map frequency

                -- pair of generators
                , map2
                    (\a b -> frequency [ a, b ])
                    randomGenerator
                    randomGenerator

                -- all generators
                , pairs
                    |> frequency
                    |> constant
                ]
                |> andThen
                    (\gen ->
                        case gen of
                            Nothing ->
                                Debug.crash "frequencyList is broken; list literal is empty"

                            Just gen ->
                                gen
                    )
    in
    map2 (,) lengthGenerator generator
        |> andThen (\( len, gen ) -> list len gen)
