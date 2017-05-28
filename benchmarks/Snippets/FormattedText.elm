module Snippets.FormattedText exposing (test1, test2)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, fuzz)


test1 : Test
test1 =
    fuzz formattedText "(passes) formatted text type" <|
        \( text, ranges ) ->
            case ranges of
                [] ->
                    Expect.pass

                range :: _ ->
                    range.start |> Expect.atMost range.end


test2 : Test
test2 =
    fuzz formattedText "(fails) formatted text type" <|
        \( text, ranges ) ->
            ranges
                |> Expect.all
                    [ List.map .start >> List.minimum >> Maybe.withDefault 0 >> Expect.atLeast 0
                    , List.map .end >> List.maximum >> Maybe.withDefault 0 >> Expect.atMost (String.length text)
                    ]


type alias FormattedText =
    ( String, List Range )


type alias Range =
    { tag : String, start : Int, end : Int }


formattedText : Fuzzer FormattedText
formattedText =
    Fuzz.map2 (,) Fuzz.string (shortList range)


range : Fuzzer Range
range =
    Fuzz.constant (\tag begin length -> Range tag (begin // 3) (begin // 3 + abs length // 3))
        |> Fuzz.andMap shortString
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap Fuzz.int


shortList : Fuzz.Fuzzer a -> Fuzz.Fuzzer (List a)
shortList itemFuzzer =
    Fuzz.frequency
        [ ( 1, Fuzz.constant [] )
        , ( 1, Fuzz.map (\a -> [ a ]) itemFuzzer )
        , ( 1, Fuzz.map2 (\a b -> [ a, b ]) itemFuzzer itemFuzzer )
        ]


shortString : Fuzzer String
shortString =
    Fuzz.string |> Fuzz.map (String.left 10)
