module DifferTest exposing (suite)

import Dict
import Differ
import Expect
import Fuzz as F exposing (Fuzzer)
import Set
import Test exposing (..)


suite : Test.Test
suite =
    describe "Round-trip tests"
        [ dictTest ()
        , setTest ()
        , dictWithListKeysTest ()
        , dictWithTupleKeysTest ()
        , listTest ()
        , productTest ()
        , complexTest ()
        , customTest ()
        ]


dictTest : () -> Test
dictTest () =
    let
        differ =
            Differ.dict Differ.string Differ.int

        fuzzer =
            dictFuzzer F.string F.int
    in
    fuzzTest fuzzer differ "dict"


dictWithListKeysTest : () -> Test
dictWithListKeysTest () =
    let
        differ =
            Differ.dict (Differ.list Differ.int) Differ.string

        fuzzer =
            dictFuzzer (F.listOfLengthBetween 0 4 F.int) F.string
    in
    fuzzTest fuzzer differ "dictWithListKeys"


dictWithTupleKeysTest : () -> Test
dictWithTupleKeysTest () =
    let
        differ =
            Differ.dict
                (Differ.pure Tuple.pair
                    |> Differ.andMap Tuple.first Differ.int
                    |> Differ.andMap Tuple.second Differ.float
                )
                Differ.string

        fuzzer =
            dictFuzzer (F.pair F.int F.float) F.string
    in
    fuzzTest fuzzer differ "dictWithTupleKeys"


setTest : () -> Test
setTest () =
    let
        differ =
            Differ.set Differ.string

        fuzzer =
            setFuzzer F.string
    in
    fuzzTest fuzzer differ "set"


listTest : () -> Test
listTest () =
    let
        differ =
            Differ.list Differ.string

        fuzzer =
            F.list F.string
    in
    fuzzTest fuzzer differ "list"


productTest : () -> Test
productTest () =
    let
        differ =
            Differ.pure (\a b c -> { a = a, b = b, c = c })
                |> Differ.andMap .a Differ.bool
                |> Differ.andMap .b Differ.bool
                |> Differ.andMap .c Differ.bool

        fuzzer =
            F.map3 (\a b c -> { a = a, b = b, c = c })
                F.bool
                F.bool
                F.bool
    in
    fuzzTest fuzzer differ "product"


complexTest : () -> Test
complexTest () =
    let
        differ =
            Differ.list
                (Differ.pure (\a b c -> { a = a, b = b, c = c })
                    |> Differ.andMap .a (Differ.dict Differ.int Differ.float)
                    |> Differ.andMap .b (Differ.set Differ.char)
                    |> Differ.andMap .c
                        (Differ.map .x (\x -> { x = x }) Differ.string)
                )

        fuzzer =
            F.listOfLengthBetween 0
                16
                (F.map3 (\a b c -> { a = a, b = b, c = c })
                    (dictFuzzer F.int (F.floatRange 0.0 1.0))
                    (setFuzzer F.char)
                    (F.map (\x -> { x = x }) F.string)
                )
    in
    fuzzTest fuzzer differ "complex"


type Custom
    = A Int
    | B String


customTest () =
    let
        differ =
            Differ.custom
                (\ok err variant ->
                    case variant of
                        A a ->
                            ok a

                        B b ->
                            err b
                )
                |> Differ.variant1 A Differ.int
                |> Differ.variant1 B Differ.string
                |> Differ.endCustom

        fuzzer =
            F.oneOf
                [ F.map A F.int
                , F.map B F.string
                ]
    in
    fuzzTest fuzzer differ "custom"



-- Helpers


dictFuzzer : Fuzzer comparable -> Fuzzer v -> Fuzzer (Dict.Dict comparable v)
dictFuzzer k v =
    F.listOfLengthBetween 0 16 (F.pair k v)
        |> F.map Dict.fromList


setFuzzer : Fuzzer comparable -> Fuzzer (Set.Set comparable)
setFuzzer m =
    F.listOfLengthBetween 0 16 m
        |> F.map Set.fromList


fuzzTest : Fuzzer b -> Differ.Differ b -> String -> Test
fuzzTest fuzzer differ name =
    fuzz2 fuzzer fuzzer name <|
        \old new ->
            let
                diff =
                    Differ.run differ old new
            in
            Differ.patch differ diff old
                |> Expect.equal (Ok new)
