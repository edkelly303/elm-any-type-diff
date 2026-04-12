module Differ exposing
    ( Differ, Changes, run, patch
    , unit, bool, int, float, char, string, dict, set, list
    , Combinator, pure, map, andMap
    , safePatch
    )

{-|


# Diffing and patching

@docs Differ, Changes, run, patch, yoloPatch


# Primitive differs

@docs unit, bool, int, float, char, string, dict, set, list


# Composing differs

@docs Combinator, pure, map, andMap

-}

import Dict exposing (Dict)
import Diff as ListDiffer
import List.Extra
import Set exposing (Set)


type alias Differ a =
    Combinator a a


type Combinator input output
    = Differ
        { index : Int
        , default : output
        , diff : input -> input -> Changes_
        , patch : Changes_ -> input -> Result Error output
        , toString : input -> String
        , fromString : String -> Maybe input
        }


type Error
    = Error


type Changes input
    = Diff Changes_


type Changes_
    = Changes (List ( Int, Changes_ ))
    | BoolChange Bool
    | IntChange Int
    | FloatChange Float
    | CharChange Char
    | StringChange String
    | DictChange DictChanges
    | SetChange SetChanges
    | ListChange ListChanges


type alias DictChanges =
    { insertions : Dict String Changes_
    , deletions : Set String
    }


type alias SetChanges =
    { insertions : List Changes_
    , deletions : List Int
    }


type alias ListChanges =
    List ListChangeType


type ListChangeType
    = Added Changes_
    | Existing Int Int



-- Use


run : Differ a -> a -> a -> Changes a
run (Differ differ) v1 v2 =
    Diff (differ.diff v1 v2)


patch : Differ a -> Changes a -> a -> a
patch differ diff old =
    safePatch differ diff old
        |> Result.withDefault old


safePatch : Differ a -> Changes a -> a -> Result Error a
safePatch (Differ differ) (Diff changes) v1 =
    differ.patch changes v1



-- Primitives


unit : Differ ()
unit =
    Differ
        { index = 0
        , default = ()
        , diff =
            \_ _ ->
                Changes []
        , patch =
            \changes _ ->
                case changes of
                    Changes [] ->
                        Ok ()

                    _ ->
                        Err Error
        , toString = \() -> "()"
        , fromString = \_ -> Just ()
        }


bool : Differ Bool
bool =
    Differ
        { index = 0
        , default = True
        , diff =
            \oldBool newBool ->
                if oldBool == newBool then
                    Changes []

                else
                    BoolChange newBool
        , patch =
            \changes oldBool ->
                case changes of
                    BoolChange newBool ->
                        Ok newBool

                    Changes [] ->
                        Ok oldBool

                    _ ->
                        Err Error
        , toString = \_ -> ""
        , fromString = \_ -> Just True
        }


char : Differ Char
char =
    Differ
        { index = 0
        , default = ' '
        , diff =
            \oldChar newChar ->
                if oldChar == newChar then
                    Changes []

                else
                    CharChange newChar
        , patch =
            \changes oldChar ->
                case changes of
                    CharChange newChar ->
                        Ok newChar

                    Changes [] ->
                        Ok oldChar

                    _ ->
                        Err Error
        , toString = String.fromChar
        , fromString = String.uncons >> Maybe.map Tuple.first
        }


float : Differ Float
float =
    Differ
        { index = 0
        , default = 0
        , diff =
            \oldFloat newFloat ->
                if oldFloat == newFloat then
                    Changes []

                else
                    FloatChange newFloat
        , patch =
            \changes oldFloat ->
                case changes of
                    FloatChange newFloat ->
                        Ok newFloat

                    Changes [] ->
                        Ok oldFloat

                    _ ->
                        Err Error
        , toString = String.fromFloat
        , fromString = String.toFloat
        }


int : Differ Int
int =
    Differ
        { index = 0
        , default = 0
        , diff =
            \oldInt newInt ->
                if oldInt == newInt then
                    Changes []

                else
                    IntChange newInt
        , patch =
            \changes oldInt ->
                case changes of
                    IntChange newInt ->
                        Ok newInt

                    Changes [] ->
                        Ok oldInt

                    _ ->
                        Err Error
        , toString = String.fromInt
        , fromString = String.toInt
        }


string : Differ String
string =
    Differ
        { index = 0
        , default = ""
        , diff =
            \oldString newString ->
                if oldString == newString then
                    Changes []

                else
                    StringChange newString
        , patch =
            \changes oldString ->
                case changes of
                    StringChange newString ->
                        Ok newString

                    Changes [] ->
                        Ok oldString

                    _ ->
                        Err Error
        , toString = identity
        , fromString = Just
        }


dict : Differ comparable -> Differ value -> Differ (Dict comparable value)
dict (Differ { toString, fromString }) (Differ valueDiffer) =
    Differ
        { index = 0
        , default = Dict.empty
        , diff =
            \oldDict newDict ->
                if oldDict == newDict then
                    Changes []

                else
                    Dict.merge
                        (\k _ out -> { out | deletions = Set.insert (toString k) out.deletions })
                        (\k oldValue newValue out ->
                            let
                                valueDiff =
                                    valueDiffer.diff oldValue newValue
                            in
                            if valueDiff == Changes [] then
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
                        |> DictChange
        , patch =
            \changes oldDict ->
                case changes of
                    DictChange { insertions, deletions } ->
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
                                    (\k insertionChanges out ->
                                        case fromString k of
                                            Just comparableK ->
                                                Dict.update comparableK
                                                    (\maybeOldValue ->
                                                        case maybeOldValue of
                                                            Just oldValue ->
                                                                valueDiffer.patch insertionChanges oldValue
                                                                    |> Result.toMaybe

                                                            Nothing ->
                                                                valueDiffer.patch insertionChanges valueDiffer.default
                                                                    |> Result.toMaybe
                                                    )
                                                    out

                                            Nothing ->
                                                out
                                    )
                                    newDictAfterDeletions
                                    insertions
                        in
                        Ok newDictAfterDeletionsAndInsertions

                    Changes [] ->
                        Ok oldDict

                    _ ->
                        Err Error
        , toString = always ""
        , fromString = always (Just Dict.empty)
        }


set : Differ comparable -> Differ (Set comparable)
set (Differ itemDiffer) =
    Differ
        { index = 0
        , default = Set.empty
        , diff =
            \oldSet newSet ->
                if oldSet == newSet then
                    Changes []

                else
                    SetChange
                        { insertions =
                            Set.diff newSet oldSet
                                |> Set.toList
                                |> List.map (itemDiffer.diff itemDiffer.default)
                        , deletions =
                            oldSet
                                |> Set.toList
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
                    SetChange { insertions, deletions } ->
                        let
                            newSetAfterDeletions =
                                oldSet
                                    |> Set.toList
                                    |> List.Extra.indexedFoldl
                                        (\idx item out ->
                                            if List.member idx deletions then
                                                Set.remove item out

                                            else
                                                out
                                        )
                                        oldSet

                            newSetAfterDeletionsAndInsertions =
                                List.foldl
                                    (\insertionChanges out ->
                                        case itemDiffer.patch insertionChanges itemDiffer.default of
                                            Ok item ->
                                                Set.insert item out

                                            Err _ ->
                                                out
                                    )
                                    newSetAfterDeletions
                                    insertions
                        in
                        Ok newSetAfterDeletionsAndInsertions

                    Changes [] ->
                        Ok oldSet

                    _ ->
                        Err Error
        , toString = always ""
        , fromString = always (Just Set.empty)
        }


list : Differ a -> Differ (List a)
list (Differ itemDiffer) =
    Differ
        { index = 0
        , default = []
        , diff =
            \oldList newList ->
                if oldList == newList then
                    Changes []

                else
                    ListChange
                        (ListDiffer.diff oldList newList
                            |> List.foldl
                                (\change { oldIdx, out } ->
                                    case change of
                                        ListDiffer.Added a ->
                                            { oldIdx = oldIdx
                                            , out = Just (Added (itemDiffer.diff itemDiffer.default a)) :: out
                                            }

                                        ListDiffer.Removed _ ->
                                            { oldIdx = oldIdx + 1
                                            , out = Nothing :: out
                                            }

                                        ListDiffer.Similar _ _ ever ->
                                            never ever

                                        ListDiffer.NoChange _ ->
                                            { oldIdx = oldIdx + 1
                                            , out =
                                                case out of
                                                    (Just (Existing prevStart _)) :: rest ->
                                                        Just (Existing prevStart oldIdx) :: rest

                                                    _ ->
                                                        Just (Existing oldIdx oldIdx) :: out
                                            }
                                )
                                { oldIdx = 0, out = [] }
                            |> .out
                            |> List.filterMap identity
                        )
        , patch =
            \changes oldList ->
                case changes of
                    ListChange cs ->
                        Ok
                            (List.foldl
                                (\change out ->
                                    case change of
                                        Added itemDiff ->
                                            (itemDiffer.default
                                                |> itemDiffer.patch itemDiff
                                                |> Result.toMaybe
                                                |> Maybe.map List.singleton
                                            )
                                                :: out

                                        Existing start end ->
                                            (oldList
                                                |> List.drop start
                                                |> List.take (1 + end - start)
                                                |> Just
                                            )
                                                :: out
                                )
                                []
                                cs
                                |> List.filterMap identity
                                |> List.concat
                            )

                    Changes [] ->
                        Ok oldList

                    _ ->
                        Err Error
        , toString = always ""
        , fromString = always (Just [])
        }



-- Composition


pure : output -> Combinator input output
pure v =
    Differ
        { index = 0
        , default = v
        , diff =
            \_ _ ->
                Changes []
        , patch =
            \_ _ ->
                Ok v
        , toString = always ""
        , fromString = always Nothing
        }


andMap :
    (input -> field)
    -> Combinator field field
    -> Combinator input (field -> output)
    -> Combinator input output
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
                if thisValue == Changes [] then
                    prevValue

                else
                    case prevValue of
                        Changes prevValues ->
                            Changes (( prev.index + 1, thisValue ) :: prevValues)

                        _ ->
                            Changes [ ( prev.index + 1, thisValue ), ( prev.index, prevValue ) ]
        , patch =
            \changes old ->
                case changes of
                    Changes ((( thisIdx, thisPatch ) :: prevPatches) as patches) ->
                        let
                            thisOld =
                                getter old

                            ( maybeCtor, maybeThisValue ) =
                                if thisIdx == prev.index + 1 then
                                    ( prev.patch (Changes prevPatches) old
                                    , this.patch thisPatch thisOld
                                    )

                                else
                                    ( prev.patch (Changes patches) old
                                    , Ok thisOld
                                    )
                        in
                        Result.map2
                            (\ctor thisValue -> ctor thisValue)
                            maybeCtor
                            maybeThisValue

                    Changes [] ->
                        let
                            maybeCtor =
                                prev.patch (Changes []) old

                            thisValue =
                                getter old
                        in
                        Result.map
                            (\ctor -> ctor thisValue)
                            maybeCtor

                    _ ->
                        Err Error
        , toString = always ""
        , fromString = always Nothing
        }


map :
    (output -> input)
    -> (input -> output)
    -> Combinator input input
    -> Combinator output output
map getter setter d =
    pure setter |> andMap getter d
