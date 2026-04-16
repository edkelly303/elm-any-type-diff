module Differ exposing
    ( Differ, Delta, Error(..), run, patch
    , unit, bool, int, float, char, string, dict, set, list
    , Combinator, pure, map, andMap
    , Custom, custom, variant1, endCustom
    , test
    )

{-|


# Diffing and patching

@docs Differ, Delta, Error, run, patch


# Primitive differs

@docs unit, bool, int, float, char, string, dict, set, list


# Composing differs


## Record, tuple and triple types (product types)

@docs Combinator, pure, map, andMap


## Custom types (sum types)

@docs Custom, custom, variant1, endCustom


## Testing

@docs test

-}

import Dict exposing (Dict)
import Diff as ListDiffer
import FNV1a
import Helpers
import List.Extra
import NestedTuple as NT
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


{-| The core type for this package: under the cover, it's a set of functions
that we can pass to `run` or `patch` to diff or patch Elm values.
-}
type alias Differ a =
    Combinator a a


{-| A special type of `Differ` used for creating product types (records, tuples,
triples).
-}
type Combinator input output
    = Differ
        { index : Int
        , default : output
        , diff : input -> input -> Changes
        , patch : Changes -> input -> Result Error output
        , toString : input -> String
        , parser : Parser output
        }


{-| A special type of `Differ` used for creating custom/union/sum types.
-}
type Custom dtor ctors differs blank getters setters makeDestructor makeDiff makePatch makeToString a
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
        , makeToString : makeToString
        , parsers : List (Parser a)
        }


{-| Possible errors that can occur during a `patch`.

If you run a `Differ` on one value and then try to apply the `Delta` to a
different value, you'll get a `MismatchedDelta` error.

If there is a bug in the implementation of this package, you'll get a
`FatalError`.

-}
type Error
    = FatalError
    | MismatchedDelta


{-| A type that represents the delta between two values. Generated
by `run`, consumed by `patch`.
-}
type Delta input
    = Delta Int Changes


type Changes
    = Changes (List ( Int, Changes ))
    | CustomChanges Int Changes
    | BoolChange Bool
    | IntChange Int
    | FloatChange Float
    | CharChange Char
    | StringChange String
    | DictChange DictChanges
    | SetChange SetChanges
    | ListChange (List ListChange)


type alias DictChanges =
    { insertions : Dict String Changes
    , deletions : Set String
    }


type alias SetChanges =
    { insertions : List Changes
    , deletions : List Int
    }


type ListChange
    = Added Changes
    | Moved Int
    | Updated Int Changes
    | Existing Int Int



-- Use


{-| Run a `Differ` over two values of the same type, and generate a `Delta`
value, which represents the changes between the first and second
values.
-}
run : Differ a -> a -> a -> Delta a
run (Differ differ) old new =
    let
        hash =
            old
                |> differ.toString
                |> FNV1a.hash
    in
    Delta hash (differ.diff old new)


{-| Use a `Delta` value to patch a value. Return a `Result` in case there are
any errors during the patching process.
-}
patch : Differ a -> Delta a -> a -> Result Error a
patch (Differ differ) (Delta deltaHash changes) old =
    let
        oldHash =
            FNV1a.hash (differ.toString old)
    in
    if deltaHash == oldHash then
        differ.patch changes old

    else
        Err MismatchedDelta



-- Primitives


{-| Differ for the `()` unit value.
-}
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
                        Err FatalError
        , toString = \() -> "()"
        , parser = Parser.succeed ()
        }


{-| Differ for `Bool` values.
-}
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
                        Err FatalError
        , toString =
            \b ->
                if b then
                    "1"

                else
                    "0"
        , parser =
            Parser.oneOf
                [ Helpers.chompOnly '1' |> Parser.map (always True)
                , Helpers.chompOnly '0' |> Parser.map (always False)
                ]
        }


{-| Differ for `Char` values.
-}
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
                        Err FatalError
        , toString = \c -> String.fromList [ '\'', c, '\'' ]
        , parser =
            Parser.succeed identity
                |. Helpers.chompOnly '\''
                |= (Parser.chompIf (always True) |> Parser.getChompedString)
                |. Helpers.chompOnly '\''
                |> Parser.andThen
                    (\str ->
                        case String.uncons str of
                            Just ( char_, _ ) ->
                                Parser.succeed char_

                            Nothing ->
                                Parser.problem "Not a char"
                    )
        }


{-| Differ for `Float` values.
-}
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
                        Err FatalError
        , toString = String.fromFloat
        , parser = Helpers.floatParser
        }


{-| Differ for `Int` values.
-}
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
                        Err FatalError
        , toString = String.fromInt
        , parser =
            Parser.oneOf
                [ Parser.succeed negate
                    |. Parser.symbol "-"
                    |= Parser.int
                , Parser.int
                ]
        }


{-| Differ for `String` values.
-}
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
                        Err FatalError
        , toString = Helpers.quoteString
        , parser = Helpers.quotedStringParser '/' '<'
        }


{-| Differ for `Dict` values.

_Warning_: Be careful if using `Float`s as keys in your `Dict`s. `NaN`, `Infinity`, and
numbers larger than 9,007,199,254,740,991 may not work as expected.

-}
dict : Differ comparable -> Differ value -> Differ (Dict comparable value)
dict (Differ keyDiffer) (Differ valueDiffer) =
    let
        opener =
            "d["

        closer =
            "]"

        separator =
            ","
    in
    Differ
        { index = 0
        , default = Dict.empty
        , diff =
            \oldDict newDict ->
                if oldDict == newDict then
                    Changes []

                else
                    Dict.merge
                        (\k _ out -> { out | deletions = Set.insert (keyDiffer.toString k) out.deletions })
                        (\k oldValue newValue out ->
                            let
                                valueDiff =
                                    valueDiffer.diff oldValue newValue
                            in
                            if valueDiff == Changes [] then
                                out

                            else
                                { out | insertions = Dict.insert (keyDiffer.toString k) valueDiff out.insertions }
                        )
                        (\k newValue out ->
                            let
                                valueDiff =
                                    valueDiffer.diff valueDiffer.default newValue
                            in
                            { out | insertions = Dict.insert (keyDiffer.toString k) valueDiff out.insertions }
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
                                        case Parser.run keyDiffer.parser k of
                                            Ok comparableK ->
                                                Dict.remove comparableK out

                                            Err _ ->
                                                out
                                    )
                                    oldDict
                                    deletions

                            newDictAfterDeletionsAndInsertions =
                                Dict.foldl
                                    (\k insertionChanges out ->
                                        case Parser.run keyDiffer.parser k of
                                            Ok comparableK ->
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

                                            Err _ ->
                                                out
                                    )
                                    newDictAfterDeletions
                                    insertions
                        in
                        Ok newDictAfterDeletionsAndInsertions

                    Changes [] ->
                        Ok oldDict

                    _ ->
                        Err FatalError
        , toString =
            \d ->
                let
                    contents =
                        Dict.toList d
                            |> List.map
                                (\( k, v ) ->
                                    [ keyDiffer.toString k
                                    , separator
                                    , valueDiffer.toString v
                                    ]
                                        |> String.concat
                                        |> Helpers.bracket "(" ")"
                                )
                            |> String.join separator
                in
                Helpers.bracket opener closer contents
        , parser =
            Parser.sequence
                { start = opener
                , separator = separator
                , end = closer
                , spaces = Parser.spaces
                , item =
                    Parser.succeed Tuple.pair
                        |. Parser.token "("
                        |= keyDiffer.parser
                        |. Parser.token separator
                        |= valueDiffer.parser
                        |. Parser.token ")"
                , trailing = Parser.Forbidden
                }
                |> Parser.map Dict.fromList
        }


{-| Differ for `Set` values.
-}
set : Differ comparable -> Differ (Set comparable)
set (Differ itemDiffer) =
    let
        opener =
            "s["

        closer =
            "]"

        separator =
            ","
    in
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
                        Err FatalError
        , toString =
            \s ->
                let
                    contents =
                        Set.toList s
                            |> List.map itemDiffer.toString
                            |> String.join separator
                in
                Helpers.bracket opener closer contents
        , parser =
            Parser.sequence
                { start = opener
                , separator = separator
                , end = closer
                , spaces = Parser.spaces
                , item = itemDiffer.parser
                , trailing = Parser.Forbidden
                }
                |> Parser.map Set.fromList
        }


{-| Differ for `List` values.
-}
list : Differ a -> Differ (List a)
list (Differ itemDiffer) =
    let
        opener =
            "l["

        closer =
            "]"

        separator =
            ","
    in
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
                                            , out =
                                                case List.Extra.elemIndex newItem oldList of
                                                    Just oldIdx ->
                                                        Just (Moved oldIdx) :: out

                                                    Nothing ->
                                                        Just (Added (itemDiffer.diff itemDiffer.default newItem)) :: out
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

                                        Moved idx ->
                                            (List.Extra.getAt idx oldList
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
                        Err FatalError
        , toString =
            \l ->
                let
                    contents =
                        l
                            |> List.map itemDiffer.toString
                            |> String.join separator
                in
                Helpers.bracket opener closer contents
        , parser =
            Parser.sequence
                { start = opener
                , separator = separator
                , end = closer
                , spaces = Parser.spaces
                , item = itemDiffer.parser
                , trailing = Parser.Forbidden
                }
        }


areSimilar : Differ a -> a -> a -> Maybe Changes
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


size : Changes -> Int
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

                        Moved _ ->
                            1

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


{-| Begin defining a `Differ` for a custom type
-}
custom :
    dtor
    ->
        Custom
            dtor
            (a7 -> a7)
            (a6 -> a6)
            (a5 -> a5)
            { appendToGetters : getters -> getters, focus : focus1 -> focus1 }
            { appendToSetters : setters -> setters, focus : focus -> focus }
            (a4 -> a4)
            (a3 -> a3)
            (a2 -> a2)
            (a1 -> a1)
            a
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
        , makeToString = NT.define
        , parsers = []
        }


{-| Add a one-argument variant to the definition of a `Differ` for a custom
type.
-}
variant1 :
    (a2 -> a)
    -> Combinator input a2
    ->
        Custom
            dtor
            (( a2 -> a, tail5 ) -> toAppender2)
            (( { default : a2
               , diff : input -> input -> Changes
               , index : Int
               , parser : Parser.Parser a2
               , patch : Changes -> input -> Result Error a2
               , toString : input -> String
               }
             , tail4
             )
             -> toAppender1
            )
            (( Maybe a6, tail3 ) -> toAppender)
            { appendToGetters : ( tuple1 -> head1, nextGetters ) -> toGetters
            , focus : tuple1 -> ( head1, tail2 )
            }
            { appendToSetters : ( head -> tuple -> tuple, nextSetters ) -> toSetters
            , focus : (( head, tail1 ) -> ( head, tail1 )) -> tuple -> tuple
            }
            (({ e | blank : b, dtor : (a5 -> c) -> d }
              -> ( Maybe a5 -> b -> c, tail )
              -> accForNext3
             )
             -> toFolder
            )
            (({ g | idx : Int, new : f, old : f, out : Maybe Changes }
              -> ( f -> Maybe a4, tailA2 )
              -> ( { h | default : a4, diff : a4 -> a4 -> Changes }, tailB2 )
              -> accForNext2
             )
             -> toFolder2_2
            )
            (({ k
                | change : i
                , idx : number
                , old : j
                , out : Result error1 a
                , selectedIdx : number
              }
              -> ( j -> Maybe a3, tailA1 )
              -> ( { l | default : a3, patch : i -> a3 -> Result Error a2 }, tailB1 )
              -> accForNext1
             )
             -> toFolder2_1
            )
            (({ n | idx : Int, out : Result error String, value : m }
              -> ( m -> Maybe a1, tailA )
              -> ( { o | toString : a1 -> String }, tailB )
              -> accForNext
             )
             -> toFolder2
            )
            a
    ->
        Custom
            dtor
            (tail5 -> toAppender2)
            (tail4 -> toAppender1)
            (tail3 -> toAppender)
            { appendToGetters : nextGetters -> toGetters, focus : tuple1 -> tail2 }
            { appendToSetters : nextSetters -> toSetters
            , focus : (tail1 -> tail1) -> tuple -> tuple
            }
            (({ blank : b, dtor : d } -> tail -> accForNext3) -> toFolder)
            (({ idx : Int, new : f, old : f, out : Maybe Changes }
              -> tailA2
              -> tailB2
              -> accForNext2
             )
             -> toFolder2_2
            )
            (({ change : i
              , idx : number
              , old : j
              , out : Result Error a
              , selectedIdx : number
              }
              -> tailA1
              -> tailB1
              -> accForNext1
             )
             -> toFolder2_1
            )
            (({ idx : Int, out : Result error String, value : m }
              -> tailA
              -> tailB
              -> accForNext
             )
             -> toFolder2
            )
            a
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

                            Err _ ->
                                if idx == selectedIdx then
                                    getter old
                                        |> Maybe.withDefault differ.default
                                        |> differ.patch change
                                        |> Result.map ctor

                                else
                                    Err FatalError
                    }
                )
                prev.makePatch
        , makeToString =
            NT.folder2
                (\getter differ { idx, value, out } ->
                    { idx = idx + 1
                    , value = value
                    , out =
                        case getter value of
                            Just v ->
                                Ok
                                    (String.concat
                                        [ String.fromInt idx
                                        , ":"
                                        , differ.toString v
                                        ]
                                    )

                            Nothing ->
                                out
                    }
                )
                prev.makeToString
        , parsers = Parser.map ctor this.parser :: prev.parsers
        }


{-| Complete the definition of a `Differ` for a custom type.
-}
endCustom :
    Custom
        dtor
        (() -> ( a -> output, b1 ))
        (() -> ( { c | default : a }, b ))
        (() -> appender)
        { appendToGetters : () -> getters, focus : focus1 }
        { appendToSetters : () -> setters, focus : focus }
        ((acc3 -> empty3 -> acc3)
         -> { blank : appender, dtor : dtor }
         -> setters
         -> { e | dtor : input -> d }
        )
        ((acc2 -> empty2 -> empty2 -> acc2)
         -> { idx : number, new : d, old : d, out : Maybe a1 }
         -> getters
         -> ( { c | default : a }, b )
         -> { f | out : Maybe Changes }
        )
        ((acc1 -> empty1 -> empty1 -> acc1)
         ->
            { change : Changes
            , idx : number1
            , old : d
            , out : Result Error value1
            , selectedIdx : Int
            }
         -> getters
         -> ( { c | default : a }, b )
         -> { g | out : Result Error output }
        )
        ((acc -> empty -> empty -> acc)
         -> { idx : number2, out : Result Error value, value : d }
         -> getters
         -> ( { c | default : a }, b )
         -> { h | out : Result x String }
        )
        output
    -> Combinator input output
endCustom (Custom prev) =
    let
        opener =
            "c{"

        closer =
            "}"

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

        defaultCtor =
            Tuple.first ctors

        default =
            differs
                |> Tuple.first
                |> .default
                |> defaultCtor

        makePatch =
            NT.endFolder2 prev.makePatch

        makeToString =
            NT.endFolder2 prev.makeToString
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
                            , out = Err FatalError
                            }
                            getters
                            differs
                            |> .out

                    _ ->
                        Err FatalError
        , toString =
            \c ->
                makeToString
                    { idx = 0
                    , value = destructor c
                    , out = Err FatalError
                    }
                    getters
                    differs
                    |> .out
                    |> Result.withDefault ""
                    |> Helpers.bracket opener closer
        , parser =
            Parser.succeed identity
                |. Parser.token opener
                |= (Parser.int
                        |> Parser.andThen
                            (\idx ->
                                Parser.succeed identity
                                    |. Parser.token ":"
                                    |= (prev.parsers
                                            |> List.reverse
                                            |> List.Extra.getAt idx
                                            |> Maybe.withDefault (Parser.problem "invalid index")
                                       )
                            )
                   )
                |. Parser.token closer
        }


{-| Begin the definition of a `Differ` for a product type (a record, tuple or triple).
-}
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
        , parser = Parser.succeed output
        }


{-| Add a field/element to the definition of a `Differ` for a product type.
-}
andMap :
    (input -> field)
    -> Combinator field field
    -> Combinator input (field -> output)
    -> Combinator input output
andMap getter (Differ this) (Differ prev) =
    let
        opener =
            "p{"

        closer =
            "}"

        separator =
            ","
    in
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
                        Err FatalError
        , toString =
            \r ->
                let
                    ( prevString, separator_ ) =
                        case prev.toString r of
                            "" ->
                                ( ""
                                , ""
                                )

                            bracketed ->
                                ( Helpers.unbracket opener closer bracketed
                                , separator
                                )

                    thisString =
                        String.concat
                            [ String.fromInt prev.index
                            , ":"
                            , getter r |> this.toString
                            ]
                in
                Helpers.bracket opener closer (prevString ++ separator_ ++ thisString)
        , parser =
            prev.parser
                |= (Parser.succeed identity
                        |. Parser.oneOf [ Parser.token opener, Parser.succeed () ]
                        |. Parser.int
                        |. Parser.token ":"
                        |= this.parser
                        |. Parser.oneOf
                            [ Parser.token closer
                            , Parser.token separator
                            ]
                   )
        }


{-| Map the value of a `Differ`
-}
map :
    (output -> input)
    -> (input -> output)
    -> Combinator input input
    -> Combinator output output
map getter setter differ =
    pure setter |> andMap getter differ


{-| Expose the internals of a `Differ` - only used for testing this package.
-}
test :
    Combinator input output
    ->
        { index : Int
        , default : output
        , diff : input -> input -> Changes
        , patch : Changes -> input -> Result Error output
        , toString : input -> String
        , parser : Parser output
        }
test (Differ differ) =
    differ
