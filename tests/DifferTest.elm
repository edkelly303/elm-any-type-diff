module DifferTest exposing (..)

import Dict
import Differ
import Expect
import Fuzz as F exposing (Fuzzer)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Round-trip tests"
        [ dictTest ()
        , setTest ()
        , listTest ()
        , complexTest ()
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
                    |> Differ.andMap .c Differ.string
                )

        fuzzer =
            F.list
                (F.map3 (\a b c -> { a = a, b = b, c = c })
                    (dictFuzzer F.int (F.floatRange 0.0 1.0))
                    (setFuzzer F.char)
                    F.string
                )
    in
    fuzzTest fuzzer differ "complex"



-- Helpers


dictFuzzer : Fuzzer comparable -> Fuzzer v -> Fuzzer (Dict.Dict comparable v)
dictFuzzer k v =
    F.list (F.pair k v)
        |> F.map Dict.fromList


setFuzzer : Fuzzer comparable -> Fuzzer (Set.Set comparable)
setFuzzer m =
    F.list m
        |> F.map Set.fromList


fuzzTest : Fuzzer b -> Differ.Differ b -> String -> Test
fuzzTest fuzzer differ name =
    fuzz2 fuzzer fuzzer name <|
        \old new ->
            let
                diff =
                    Differ.run differ old new
            in
            Differ.safePatch differ diff old
                |> Expect.equal (Ok new)
