module Example exposing (..)

import Dict
import Differ
import Expect
import Fuzz as F exposing (Fuzzer)
import Set
import Test exposing (..)


mkDictFuzzer : Fuzzer comparable -> Fuzzer v -> Fuzzer (Dict.Dict comparable v)
mkDictFuzzer k v =
    F.list (F.pair k v)
        |> F.map Dict.fromList


mkSetFuzzer : Fuzzer comparable -> Fuzzer (Set.Set comparable)
mkSetFuzzer m =
    F.list m
        |> F.map Set.fromList


dictTest : Test
dictTest =
    let
        dictDiffer =
            Differ.dict Differ.string Differ.int

        dictFuzzer =
            mkDictFuzzer F.string F.int
    in
    fuzzTest dictFuzzer dictDiffer "dict"


setTest : Test
setTest =
    let
        differ =
            Differ.set Differ.string

        fuzzer =
            mkSetFuzzer F.string
    in
    fuzzTest fuzzer differ "set"


productTest : Test
productTest =
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


fuzzTest : Fuzzer b -> Differ.Differ b -> String -> Test
fuzzTest fuzzer differ name =
    fuzz2 fuzzer fuzzer name <|
        \old new ->
            let
                diff =
                    Differ.run differ old new
            in
            Differ.patch differ diff old
                |> Expect.equal (Just new)
