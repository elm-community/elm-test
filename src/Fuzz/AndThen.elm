module Fuzz.AndThen exposing (andThen)

{-| Split out the implementation of Fuzz.andThen. We are thinking about
removing it and this makes the other file easier to work with. See #161.
-}

import Fuzz.Internal as Internal exposing (Fuzz(..), invalidReason)
import Lazy.List exposing (LazyList)
import Random.Pcg as Random exposing (Generator)
import RoseTree exposing (RoseTree(..))


type alias Fuzzer a =
    Internal.Fuzzer a


{-| Create a fuzzer based on the result of another fuzzer.
-}
andThen : (a -> Fuzzer b) -> Fuzzer a -> Fuzzer b
andThen transform (Internal.Fuzzer baseFuzzer) =
    Internal.Fuzzer
        (\noShrink ->
            case baseFuzzer noShrink of
                Gen genVal ->
                    Gen <| Random.andThen (transform >> Internal.unpackGenVal) genVal

                Shrink genTree ->
                    Shrink <| andThenRoseTrees transform genTree

                InvalidFuzzer reason ->
                    InvalidFuzzer reason
        )


andThenRoseTrees : (a -> Fuzzer b) -> Generator (RoseTree a) -> Generator (RoseTree b)
andThenRoseTrees transform genTree =
    genTree
        |> Random.andThen
            (\(Rose root branches) ->
                let
                    genOtherChildren : Generator (LazyList (RoseTree b))
                    genOtherChildren =
                        branches
                            |> Lazy.List.map (\rt -> RoseTree.map (transform >> Internal.unpackGenTree) rt |> unwindRoseTree)
                            |> unwindLazyList
                            |> Random.map (Lazy.List.map RoseTree.flatten)
                in
                Random.map2
                    (\(Rose trueRoot rootsChildren) otherChildren ->
                        Rose trueRoot (Lazy.List.append rootsChildren otherChildren)
                    )
                    (Internal.unpackGenTree (transform root))
                    genOtherChildren
            )


unwindRoseTree : RoseTree (Generator a) -> Generator (RoseTree a)
unwindRoseTree (Rose genRoot lazyListOfRoseTreesOfGenerators) =
    case Lazy.List.headAndTail lazyListOfRoseTreesOfGenerators of
        Nothing ->
            Random.map RoseTree.singleton genRoot

        Just ( Rose gen children, moreList ) ->
            Random.map4 (\a b c d -> Rose a (Lazy.List.cons (Rose b c) d))
                genRoot
                gen
                (Lazy.List.map unwindRoseTree children |> unwindLazyList)
                (Lazy.List.map unwindRoseTree moreList |> unwindLazyList)


unwindLazyList : LazyList (Generator a) -> Generator (LazyList a)
unwindLazyList lazyListOfGenerators =
    case Lazy.List.headAndTail lazyListOfGenerators of
        Nothing ->
            Random.constant Lazy.List.empty

        Just ( head, tail ) ->
            Random.map2 Lazy.List.cons head (unwindLazyList tail)
