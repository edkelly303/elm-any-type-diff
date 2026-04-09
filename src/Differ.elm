module Differ exposing
    ( Differ, Diff, run, patch
    , unit, bool, int, float, char, string, dict, set
    , Combinator, pure, map, andMap
    )

{-|


# Diffing and patching

@docs Differ, Diff, run, patch


# Primitive differs

@docs unit, bool, int, float, char, string, dict, set


# Composing differs

@docs Combinator, pure, map, andMap

-}

import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


type alias Differ a =
    Combinator a a


type Combinator input output
    = Differ
        { index : Int
        , default : output
        , diff : input -> input -> Value
        , patch : Value -> input -> Maybe output
        , toString : input -> String
        , fromString : String -> Maybe input
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
    | SetV SetDiff


type alias DictDiff =
    { insertions : Dict String Value
    , deletions : Set String
    }


type alias SetDiff =
    { insertions : List Value
    , deletions : List Int
    }



-- Use


run : Differ a -> a -> a -> Diff a
run (Differ differ) v1 v2 =
    Diff (differ.diff v1 v2)


patch : Differ a -> Diff a -> a -> a
patch (Differ differ) (Diff diff) v1 =
    differ.patch diff v1
        |> Maybe.withDefault v1



-- Primitives


unit : Differ ()
unit =
    Differ
        { index = 0
        , default = ()
        , diff =
            \_ _ ->
                UnitV
        , patch =
            \d _ ->
                case d of
                    UnitV ->
                        Just ()

                    _ ->
                        Nothing
        , toString = \() -> "()"
        , fromString = \_ -> Just ()
        }


bool : Differ Bool
bool =
    Differ
        { index = 0
        , default = True
        , diff =
            \v1 v2 ->
                if v1 == v2 then
                    ProductV []

                else
                    BoolV v2
        , patch =
            \d v1 ->
                case d of
                    BoolV v2 ->
                        Just v2

                    ProductV [] ->
                        Just v1

                    _ ->
                        Nothing
        , toString = \_ -> ""
        , fromString = \_ -> Just True
        }


char : Differ Char
char =
    Differ
        { index = 0
        , default = ' '
        , diff =
            \v1 v2 ->
                if v1 == v2 then
                    ProductV []

                else
                    CharV v2
        , patch =
            \d v1 ->
                case d of
                    CharV v2 ->
                        Just v2

                    ProductV [] ->
                        Just v1

                    _ ->
                        Nothing
        , toString = String.fromChar
        , fromString = String.uncons >> Maybe.map Tuple.first
        }


float : Differ Float
float =
    Differ
        { index = 0
        , default = 0
        , diff =
            \v1 v2 ->
                if v1 == v2 then
                    ProductV []

                else
                    FloatV v2
        , patch =
            \d v1 ->
                case d of
                    FloatV v2 ->
                        Just v2

                    ProductV [] ->
                        Just v1

                    _ ->
                        Nothing
        , toString = String.fromFloat
        , fromString = String.toFloat
        }


int : Differ Int
int =
    Differ
        { index = 0
        , default = 0
        , diff =
            \v1 v2 ->
                if v1 == v2 then
                    ProductV []

                else
                    IntV v2
        , patch =
            \d v1 ->
                case d of
                    IntV v2 ->
                        Just v2

                    ProductV [] ->
                        Just v1

                    _ ->
                        Nothing
        , toString = String.fromInt
        , fromString = String.toInt
        }


string : Differ String
string =
    Differ
        { index = 0
        , default = ""
        , diff =
            \s1 s2 ->
                if s1 == s2 then
                    ProductV []

                else
                    StringV s2
        , patch =
            \d s1 ->
                case d of
                    StringV s2 ->
                        Just s2

                    ProductV [] ->
                        Just s1

                    _ ->
                        Nothing
        , toString = identity
        , fromString = Just
        }


dict :
    Differ comparable
    -> Differ v
    -> Differ (Dict comparable v)
dict (Differ { toString, fromString }) (Differ valueDiffer) =
    Differ
        { index = 0
        , default = Dict.empty
        , diff =
            \oldDict newDict ->
                if oldDict == newDict then
                    ProductV []

                else
                    Dict.merge
                        (\k _ out -> { out | deletions = Set.insert (toString k) out.deletions })
                        (\k oldValue newValue out ->
                            let
                                valueDiff =
                                    valueDiffer.diff oldValue newValue
                            in
                            if valueDiff == ProductV [] then
                                out

                            else
                                { out | insertions = Dict.insert (toString k) valueDiff out.insertions }
                        )
                        (\k newValue out ->
                            let
                                valueDiff =
                                    valueDiffer.diff valueDiffer.default newValue
                            in
                            { out | insertions = Dict.insert (toString k) valueDiff out.insertions }
                        )
                        oldDict
                        newDict
                        { insertions = Dict.empty, deletions = Set.empty }
                        |> DictV
        , patch =
            \d oldDict ->
                case d of
                    DictV { insertions, deletions } ->
                        let
                            newDictAfterDeletions =
                                Set.foldl
                                    (\k out ->
                                        case fromString k of
                                            Just comparableK ->
                                                Dict.remove comparableK out

                                            Nothing ->
                                                out
                                    )
                                    oldDict
                                    deletions

                            newDictAfterDeletionsAndInsertions =
                                Dict.foldl
                                    (\k v out ->
                                        case fromString k of
                                            Just comparableK ->
                                                Dict.update comparableK
                                                    (\maybeV1 ->
                                                        case maybeV1 of
                                                            Just v1_ ->
                                                                valueDiffer.patch v v1_

                                                            Nothing ->
                                                                valueDiffer.patch v valueDiffer.default
                                                    )
                                                    out

                                            Nothing ->
                                                out
                                    )
                                    newDictAfterDeletions
                                    insertions
                        in
                        Just newDictAfterDeletionsAndInsertions

                    ProductV [] ->
                        Just oldDict

                    _ ->
                        Nothing
        , toString = always ""
        , fromString = always (Just Dict.empty)
        }


set :
    Differ comparable
    -> Differ (Set comparable)
set (Differ itemDiffer) =
    Differ
        { index = 0
        , default = Set.empty
        , diff =
            \oldSet newSet ->
                if oldSet == newSet then
                    ProductV []

                else
                    SetV
                        { insertions =
                            Set.diff oldSet newSet
                                |> Set.toList
                                |> List.map (itemDiffer.diff itemDiffer.default)
                        , deletions =
                            Set.toList oldSet
                                |> List.indexedMap
                                    (\idx item ->
                                        if Set.member item newSet then
                                            Nothing

                                        else
                                            Just idx
                                    )
                                |> List.filterMap identity
                        }
        , patch =
            \changes oldSet ->
                case changes of
                    SetV { insertions, deletions } ->
                        let
                            newSetAfterDeletions =
                                oldSet
                                    |> Set.toList
                                    |> List.Extra.indexedFoldl
                                        (\idx item out ->
                                            if List.member idx deletions then
                                                out

                                            else
                                                Set.insert item out
                                        )
                                        Set.empty

                            newSetAfterDeletionsAndInsertions =
                                List.foldl
                                    (\insertion out ->
                                        case itemDiffer.patch insertion itemDiffer.default of
                                            Just item ->
                                                Set.insert item out

                                            Nothing ->
                                                out
                                    )
                                    newSetAfterDeletions
                                    insertions
                        in
                        Just newSetAfterDeletionsAndInsertions

                    ProductV [] ->
                        Just oldSet

                    _ ->
                        Nothing
        , toString = always ""
        , fromString = always (Just Set.empty)
        }



-- Composition


pure : output -> Combinator input output
pure v =
    Differ
        { index = 0
        , default = v
        , diff =
            \_ _ ->
                ProductV []
        , patch =
            \_ _ ->
                Just v
        , toString = always ""
        , fromString = always Nothing
        }


andMap : (input -> field) -> Combinator field field -> Combinator input (field -> output) -> Combinator input output
andMap getter (Differ this) (Differ prev) =
    Differ
        { index = prev.index + 1
        , default = prev.default this.default
        , diff =
            \v1 v2 ->
                let
                    thisValue =
                        this.diff (getter v1) (getter v2)

                    prevValue =
                        prev.diff v1 v2
                in
                if thisValue == ProductV [] then
                    prevValue

                else
                    case prevValue of
                        ProductV prevValues ->
                            ProductV (( prev.index + 1, thisValue ) :: prevValues)

                        _ ->
                            ProductV [ ( prev.index + 1, thisValue ), ( prev.index, prevValue ) ]
        , patch =
            \d v1 ->
                case d of
                    ProductV ((( thisIdx, thisPatch ) :: prevPatches) as patches) ->
                        let
                            thisV1 =
                                getter v1

                            ( maybeCtor, thisValue ) =
                                if thisIdx == prev.index + 1 then
                                    ( prev.patch (ProductV prevPatches) v1
                                    , this.patch thisPatch thisV1
                                        |> Maybe.withDefault thisV1
                                    )

                                else
                                    ( prev.patch (ProductV patches) v1
                                    , thisV1
                                    )
                        in
                        Maybe.map (\ctor -> ctor thisValue) maybeCtor

                    ProductV [] ->
                        let
                            maybeCtor =
                                prev.patch (ProductV []) v1

                            thisValue =
                                getter v1
                        in
                        Maybe.map
                            (\ctor -> ctor thisValue)
                            maybeCtor

                    _ ->
                        Nothing
        , toString = always ""
        , fromString = always Nothing
        }


map : (output -> input) -> (input -> output) -> Combinator input input -> Combinator output output
map getter setter d =
    pure setter |> andMap getter d
