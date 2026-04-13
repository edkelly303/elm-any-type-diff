module Differ exposing
    ( Differ, Changes, Error, run, patch, safePatch
    , unit, bool, int, float, char, string, dict, set, list
    , Combinator, pure, map, andMap, custom, variant1, endCustom
    )

{-|


# Diffing and patching

@docs Differ, Changes, Error, run, patch, safePatch


# Primitive differs

@docs unit, bool, int, float, char, string, dict, set, list


# Composing differs

@docs Combinator, pure, map, andMap, custom, variant1, endCustom

-}

import Dict exposing (Dict)
import Diff as ListDiffer
import List.Extra
import NestedTuple as NT
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
    | CustomChanges Int Changes_
    | BoolChange Bool
    | IntChange Int
    | FloatChange Float
    | CharChange Char
    | StringChange String
    | DictChange DictChanges
    | SetChange SetChanges
    | ListChange (List ListChange)


type alias DictChanges =
    { insertions : Dict String Changes_
    , deletions : Set String
    }


type alias SetChanges =
    { insertions : List Changes_
    , deletions : List Int
    }


type ListChange
    = Added Changes_
    | Updated Int Changes_
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
                        (ListDiffer.diffWith (areSimilar (Differ itemDiffer)) oldList newList
                            |> List.foldl
                                (\change { idx, out } ->
                                    case change of
                                        ListDiffer.Added newItem ->
                                            { idx = idx
                                            , out = Just (Added (itemDiffer.diff itemDiffer.default newItem)) :: out
                                            }

                                        ListDiffer.Removed _ ->
                                            { idx = idx + 1
                                            , out = Nothing :: out
                                            }

                                        ListDiffer.Similar _ _ changes ->
                                            { idx = idx + 1
                                            , out = Just (Updated idx changes) :: out
                                            }

                                        ListDiffer.NoChange _ ->
                                            { idx = idx + 1
                                            , out =
                                                case out of
                                                    (Just (Existing prevStart _)) :: rest ->
                                                        Just (Existing prevStart idx) :: rest

                                                    _ ->
                                                        Just (Existing idx idx) :: out
                                            }
                                )
                                { idx = 0, out = [] }
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

                                        Updated idx itemDiff ->
                                            (List.Extra.getAt idx oldList
                                                |> Maybe.withDefault itemDiffer.default
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


areSimilar : Differ a -> a -> a -> Maybe Changes_
areSimilar (Differ itemDiffer) old new =
    let
        oldNewDiff =
            itemDiffer.diff old new

        defaultNewDiff =
            itemDiffer.diff itemDiffer.default new
    in
    if size oldNewDiff < size defaultNewDiff then
        Just oldNewDiff

    else
        Nothing


size : Changes_ -> Int
size changes =
    case changes of
        Changes cs ->
            List.map (\( _, c ) -> size c) cs
                |> List.sum

        CustomChanges _ cs ->
            size cs

        DictChange { insertions, deletions } ->
            Set.size deletions
                + (Dict.values insertions
                    |> List.map size
                    |> List.sum
                  )

        SetChange { insertions, deletions } ->
            List.length deletions
                + (List.map size insertions
                    |> List.sum
                  )

        ListChange cs ->
            List.map
                (\c ->
                    case c of
                        Added addedC ->
                            size addedC

                        Updated _ updatedC ->
                            size updatedC

                        Existing _ _ ->
                            1
                )
                cs
                |> List.sum

        BoolChange _ ->
            1

        IntChange _ ->
            1

        FloatChange _ ->
            1

        CharChange _ ->
            1

        StringChange _ ->
            1



-- Composition


type Custom dtor ctors differs blank getters setters makeDestructor makeDiff makePatch
    = Custom
        { dtor : dtor
        , ctors : ctors
        , differs : differs
        , blank : blank
        , getters : getters
        , setters : setters
        , makeDestructor : makeDestructor
        , makeDiff : makeDiff
        , makePatch : makePatch
        }


custom dtor =
    Custom
        { dtor = dtor
        , ctors = NT.define
        , differs = NT.define
        , blank = NT.define
        , getters = NT.defineGetters
        , setters = NT.defineSetters
        , makeDestructor = NT.define
        , makeDiff = NT.define
        , makePatch = NT.define
        }


variant1 ctor (Differ this) (Custom prev) =
    Custom
        { dtor = prev.dtor
        , ctors = NT.appender ctor prev.ctors
        , differs = NT.appender this prev.differs
        , blank = NT.appender Nothing prev.blank
        , getters = NT.getter prev.getters
        , setters = NT.setter prev.setters
        , makeDestructor =
            NT.folder
                (\setter acc ->
                    { blank = acc.blank
                    , dtor = acc.dtor (\v -> setter (Just v) acc.blank)
                    }
                )
                prev.makeDestructor
        , makeDiff =
            NT.folder2
                (\getter differ { old, new, idx, out } ->
                    { old = old
                    , new = new
                    , idx = idx + 1
                    , out =
                        case ( getter old, getter new ) of
                            ( Just oldV, Just newV ) ->
                                Just (CustomChanges idx (differ.diff oldV newV))

                            ( Nothing, Just newV ) ->
                                Just (CustomChanges idx (differ.diff differ.default newV))

                            _ ->
                                out
                    }
                )
                prev.makeDiff
        , makePatch =
            NT.folder2
                (\getter differ { idx, selectedIdx, change, old, out } ->
                    { idx = idx + 1
                    , selectedIdx = selectedIdx
                    , old = old
                    , change = change
                    , out =
                        case out of
                            Ok new ->
                                Ok new

                            Err Error ->
                                if idx == selectedIdx then
                                    getter old
                                        |> Maybe.withDefault differ.default
                                        |> differ.patch change
                                        |> Result.map ctor

                                else
                                    Err Error
                    }
                )
                prev.makePatch
        }


endCustom (Custom prev) =
    let
        blank =
            NT.endAppender prev.blank

        ctors =
            NT.endAppender prev.ctors

        differs =
            NT.endAppender prev.differs

        getters =
            NT.endGetters prev.getters

        setters =
            NT.endSetters prev.setters

        makeDestructor =
            NT.endFolder prev.makeDestructor

        destructor =
            makeDestructor { blank = blank, dtor = prev.dtor } setters
                |> .dtor

        makeDiff =
            NT.endFolder2 prev.makeDiff

        default =
            Tuple.first ctors (Tuple.first differs |> .default)

        makePatch =
            NT.endFolder2 prev.makePatch
    in
    Differ
        { index = 0
        , default = default
        , diff =
            \old new ->
                makeDiff
                    { old = destructor old
                    , new = destructor new
                    , idx = 0
                    , out = Nothing
                    }
                    getters
                    differs
                    |> .out
                    |> Maybe.withDefault (Changes [])
        , patch =
            \changes old ->
                case changes of
                    CustomChanges selectedIdx change ->
                        makePatch
                            { idx = 0
                            , selectedIdx = selectedIdx
                            , change = change
                            , old = destructor old
                            , out = Err Error
                            }
                            getters
                            differs
                            |> .out

                    _ ->
                        Err Error
        , toString = always ""
        , fromString = always Nothing
        }


pure : output -> Combinator input output
pure output =
    Differ
        { index = 0
        , default = output
        , diff =
            \_ _ ->
                Changes []
        , patch =
            \_ _ ->
                Ok output
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
map getter setter differ =
    pure setter |> andMap getter differ
