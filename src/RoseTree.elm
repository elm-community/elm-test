module RoseTree exposing (..)

{-| RoseTree implementation in Elm using Lazy Lists.

# Definition
@docs RoseTree

# Constructors
@docs singleton

# Query a rosetree
@docs root, children, hasChildren

# Common operations
@docs addChild

# Map-reduce et al.
@docs map, map2, andMap, map3, map4, map5, zip, zip3, zip4, zip5, flatten, flatMap, andThen, reduce, sum, product

-}

import Lazy.List as LazyList exposing (LazyList, (:::), (+++))


{-| RoseTree type.
A rosetree is a tree with a root whose children are themselves
rosetrees.
-}
type RoseTree a
    = Rose a (LazyList (RoseTree a))


{-| Make a singleton rosetree
-}
singleton : a -> RoseTree a
singleton a =
    Rose a LazyList.empty


{-| Get the root of a rosetree
-}
root : RoseTree a -> a
root (Rose a _) =
    a


{-| Get the children of a rosetree
-}
children : RoseTree a -> LazyList (RoseTree a)
children (Rose _ c) =
    c


{-| Add a child to the rosetree.
-}
addChild : RoseTree a -> RoseTree a -> RoseTree a
addChild child (Rose a c) =
    Rose a (child ::: c)


{-| Does the tree have children?
-}
hasChildren : RoseTree a -> Bool
hasChildren (Rose a c) =
    not (LazyList.isEmpty c)


{-| Map a function over a rosetree
-}
map : (a -> b) -> RoseTree a -> RoseTree b
map f (Rose a c) =
    Rose (f a) (LazyList.map (map f) c)


{-| -}
map2 : (a -> b -> c) -> RoseTree a -> RoseTree b -> RoseTree c
map2 f (Rose a ca) (Rose b cb) =
    Rose (f a b) (LazyList.map2 (map2 f) ca cb)


{-| Chain mapping operations.
-}
andMap : RoseTree (a -> b) -> RoseTree a -> RoseTree b
andMap =
    map2 (<|)


{-| -}
map3 : (a -> b -> c -> d) -> RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d
map3 f ra rb rc =
    f
        `map` ra
        `andMap` rb
        `andMap` rc


{-| -}
map4 : (a -> b -> c -> d -> e) -> RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d -> RoseTree e
map4 f ra rb rc rd =
    f
        `map` ra
        `andMap` rb
        `andMap` rc
        `andMap` rd


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d -> RoseTree e -> RoseTree f
map5 f ra rb rc rd re =
    f
        `map` ra
        `andMap` rb
        `andMap` rc
        `andMap` rd
        `andMap` re


{-| -}
zip : RoseTree a -> RoseTree b -> RoseTree ( a, b )
zip =
    map2 (,)


{-| -}
zip3 : RoseTree a -> RoseTree b -> RoseTree c -> RoseTree ( a, b, c )
zip3 =
    map3 (,,)


{-| -}
zip4 : RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d -> RoseTree ( a, b, c, d )
zip4 =
    map4 (,,,)


{-| -}
zip5 : RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d -> RoseTree e -> RoseTree ( a, b, c, d, e )
zip5 =
    map5 (,,,,)


{-| Flatten a rosetree of rosetrees.
-}
flatten : RoseTree (RoseTree a) -> RoseTree a
flatten (Rose (Rose a c) cs) =
    Rose a (c +++ LazyList.map flatten cs)


{-| -}
flatMap : (a -> RoseTree b) -> RoseTree a -> RoseTree b
flatMap f =
    map f >> flatten


{-| -}
andThen : RoseTree a -> (a -> RoseTree b) -> RoseTree b
andThen =
    flip flatMap


{-| -}
reduce : (a -> b -> b) -> b -> RoseTree a -> b
reduce reducer b (Rose a c) =
    LazyList.reduce (flip (reduce reducer)) (reducer a b) c


{-| -}
sum : RoseTree number -> number
sum =
    reduce (+) 0


{-| -}
product : RoseTree number -> number
product =
    reduce (*) 1
