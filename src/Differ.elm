module Differ exposing
    ( Diff
    , Differ
    , andMap
    , bool
    , char
    , float
    , int
    , map
    , patch
    , pure
    , run
    , string
    , unit
    )

import Dict exposing (Dict)
import Dict.Extra
import Set exposing (Set)


{-|


# Diffing and patching

@docs Differ, Diff, run, patch


# Primitive differs

@docs unit, bool, int, float, char, string


# Composing differs

@docs pure, map, andMap

-}
type Differ input output
    = Differ
        { index : Int
        , diff : input -> input -> Diff input
        , patch : Diff input -> input -> Maybe output
        }


type Diff input
    = Diff Value


type Value
    = UnitV
    | BoolV Bool
    | IntV Int
    | FloatV Float
    | CharV Char
    | StringV String
    | ProductV (List ( Int, Value ))
    | DictV DictDiff


type alias DictDiff =
    { insertions : Dict String Value
    , deletions : Set String
    }


dict :
    { keyToString : comparable -> String
    , keyFromString : String -> Maybe comparable
    , zero : v
    }
    -> Differ v v
    -> Differ (Dict comparable v) (Dict comparable v)
dict { keyToString, keyFromString, zero } (Differ value) =
    Differ
        { index = 0
        , diff =
            \v1 v2 ->
                Diff
                    (if v1 == v2 then
                        ProductV []

                     else
                        DictV { insertions = Dict.empty, deletions = Set.empty }
                    )
        , patch =
            \(Diff d) v1 ->
                case d of
                    DictV { insertions, deletions } ->
                        let
                            dels =
                                Set.foldl
                                    (\k out ->
                                        case keyFromString k of
                                            Just comparableK ->
                                                Dict.remove comparableK out

                                            Nothing ->
                                                out
                                    )
                                    v1
                                    deletions

                            ins =
                                Dict.foldl
                                    (\k v out ->
                                        case keyFromString k of
                                            Just comparableK ->
                                                Dict.update comparableK
                                                    (\maybeV1 ->
                                                        case maybeV1 of
                                                            Just v1_ ->
                                                                value.patch (Diff v) v1_

                                                            Nothing ->
                                                                value.patch (Diff v) zero
                                                    )
                                                    out

                                            Nothing ->
                                                out
                                    )
                                    dels
                                    insertions
                        in
                        Just ins

                    ProductV [] ->
                        Just v1

                    _ ->
                        Nothing
        }


emptyDictDiff : DictDiff
emptyDictDiff =
    { insertions = Dict.empty
    , deletions = Set.empty
    }



-- fromDict : Dict comparable item -> Dict comparable item -> DictDiff
-- fromDict d1 d2 =
--     Dict.merge
--         (\id _ output -> { output | deletions = Set.insert id output.deletions })
--         (\id old new output ->
--             if old /= new then
--                 { output | insertions = Dict.insert id new output.insertions }
--             else
--                 output
--         )
--         (\id new output -> { output | insertions = Dict.insert id new output.insertions })
--         d1
--         d2
--         emptyDictDiff
-- Use


run : Differ input output -> input -> input -> Diff input
run (Differ differ) v1 v2 =
    differ.diff v1 v2


patch : Differ input output -> Diff input -> input -> Maybe output
patch (Differ differ) d v1 =
    differ.patch d v1



-- Primitives


unit : Differ () ()
unit =
    Differ
        { index = 0
        , diff =
            \_ _ ->
                Diff UnitV
        , patch =
            \(Diff d) _ ->
                case d of
                    UnitV ->
                        Just ()

                    _ ->
                        Nothing
        }


bool : Differ Bool Bool
bool =
    Differ
        { index = 0
        , diff =
            \v1 v2 ->
                Diff
                    (if v1 == v2 then
                        ProductV []

                     else
                        BoolV v2
                    )
        , patch =
            \(Diff d) v1 ->
                case d of
                    BoolV v2 ->
                        Just v2

                    ProductV [] ->
                        Just v1

                    _ ->
                        Nothing
        }


char : Differ Char Char
char =
    Differ
        { index = 0
        , diff =
            \v1 v2 ->
                Diff
                    (if v1 == v2 then
                        ProductV []

                     else
                        CharV v2
                    )
        , patch =
            \(Diff d) v1 ->
                case d of
                    CharV v2 ->
                        Just v2

                    ProductV [] ->
                        Just v1

                    _ ->
                        Nothing
        }


float : Differ Float Float
float =
    Differ
        { index = 0
        , diff =
            \v1 v2 ->
                Diff
                    (if v1 == v2 then
                        ProductV []

                     else
                        FloatV v2
                    )
        , patch =
            \(Diff d) v1 ->
                case d of
                    FloatV v2 ->
                        Just v2

                    ProductV [] ->
                        Just v1

                    _ ->
                        Nothing
        }


int : Differ Int Int
int =
    Differ
        { index = 0
        , diff =
            \v1 v2 ->
                Diff
                    (if v1 == v2 then
                        ProductV []

                     else
                        IntV v2
                    )
        , patch =
            \(Diff d) v1 ->
                case d of
                    IntV v2 ->
                        Just v2

                    ProductV [] ->
                        Just v1

                    _ ->
                        Nothing
        }


string : Differ String String
string =
    Differ
        { index = 0
        , diff =
            \s1 s2 ->
                Diff
                    (if s1 == s2 then
                        ProductV []

                     else
                        StringV s2
                    )
        , patch =
            \(Diff d) s1 ->
                case d of
                    StringV s2 ->
                        Just s2

                    ProductV [] ->
                        Just s1

                    _ ->
                        Nothing
        }



-- Composition


pure : output -> Differ input output
pure v =
    Differ
        { index = 0
        , diff =
            \_ _ ->
                Diff (ProductV [])
        , patch =
            \_ _ ->
                Just v
        }


andMap : (input -> field) -> Differ field field -> Differ input (field -> output) -> Differ input output
andMap getter (Differ this) (Differ prev) =
    Differ
        { index = prev.index + 1
        , diff =
            \v1 v2 ->
                let
                    (Diff thisValue) =
                        this.diff (getter v1) (getter v2)

                    (Diff prevValue) =
                        prev.diff v1 v2
                in
                Diff
                    (if thisValue == ProductV [] then
                        prevValue

                     else
                        case prevValue of
                            ProductV prevValues ->
                                ProductV (( prev.index + 1, thisValue ) :: prevValues)

                            _ ->
                                ProductV [ ( prev.index + 1, thisValue ), ( prev.index, prevValue ) ]
                    )
        , patch =
            \(Diff d) v1 ->
                case d of
                    ProductV ((( thisIdx, thisPatch ) :: prevPatches) as patches) ->
                        let
                            thisV1 =
                                getter v1

                            ( maybeCtor, thisValue ) =
                                if thisIdx == prev.index + 1 then
                                    ( prev.patch (Diff (ProductV prevPatches)) v1
                                    , this.patch (Diff thisPatch) thisV1
                                        |> Maybe.withDefault thisV1
                                    )

                                else
                                    ( prev.patch (Diff (ProductV patches)) v1
                                    , thisV1
                                    )
                        in
                        Maybe.map (\ctor -> ctor thisValue) maybeCtor

                    ProductV [] ->
                        let
                            maybeCtor =
                                prev.patch (Diff (ProductV [])) v1

                            thisValue =
                                getter v1
                        in
                        Maybe.map
                            (\ctor -> ctor thisValue)
                            maybeCtor

                    _ ->
                        Nothing
        }


map : (output -> input) -> (input -> output) -> Differ input input -> Differ output output
map getter setter d =
    pure setter |> andMap getter d
