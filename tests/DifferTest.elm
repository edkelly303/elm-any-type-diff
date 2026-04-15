module DifferTest exposing (suite)

import Dict
import Differ
import Expect
import Fuzz exposing (Fuzzer)
import Parser
import Set
import Test exposing (Test)


suite : Test.Test
suite =
    Test.describe "Round-trip tests"
        [ stringParserTest ()
        , floatParserTest ()
        , productParserTest ()
        , customParserTest ()
        , dictTest ()
        , setTest ()
        , dictWithListKeysTest ()
        , dictWithTupleKeysTest ()
        , listTest ()
        , productTest ()
        , complexTest ()
        , customTest ()
        ]


floatParserTest : () -> Test
floatParserTest () =
    Test.fuzz myNiceFloat "float parser" <|
        \flt ->
            let
                fs =
                    Differ.test Differ.float

                str =
                    fs.toString flt
            in
            ( str
            , Parser.run fs.parser str
            )
                |> Expect.equal ( str, Ok flt )


stringParserTest : () -> Test
stringParserTest () =
    Test.fuzz Fuzz.string "string parser" <|
        \str ->
            let
                fs =
                    Differ.test Differ.string

                quoted =
                    fs.toString str
            in
            ( quoted
            , quoted
                |> Parser.run fs.parser
            )
                |> Expect.equal ( quoted, Ok str )


productParserTest : () -> Test
productParserTest () =
    Test.fuzz (Fuzz.pair Fuzz.bool Fuzz.bool) "product parser" <|
        \tup ->
            let
                fs =
                    Differ.test
                        (Differ.pure Tuple.pair
                            |> Differ.andMap Tuple.first Differ.bool
                            |> Differ.andMap Tuple.second Differ.bool
                        )

                str =
                    fs.toString tup

                out =
                    Parser.run fs.parser str
            in
            ( str, out )
                |> Expect.equal ( str, Ok tup )


customParserTest : () -> Test
customParserTest () =
    Test.fuzz
        (Fuzz.oneOf
            [ Fuzz.map A Fuzz.int
            , Fuzz.map B Fuzz.string
            ]
        )
        "custom parser"
    <|
        \cust ->
            let
                fs =
                    Differ.test
                        (Differ.custom
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
                        )

                str =
                    fs.toString cust

                out =
                    Parser.run fs.parser str
            in
            ( str, out )
                |> Expect.equal ( str, Ok cust )


dictTest : () -> Test
dictTest () =
    let
        differ =
            Differ.dict Differ.string Differ.int

        fuzzer =
            dictFuzzer Fuzz.string Fuzz.int
    in
    fuzzTest fuzzer differ "dict"


dictWithListKeysTest : () -> Test
dictWithListKeysTest () =
    let
        differ =
            Differ.dict (Differ.list Differ.int) Differ.string

        fuzzer =
            dictFuzzer (Fuzz.listOfLengthBetween 0 4 Fuzz.int) Fuzz.string
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
            dictFuzzer (Fuzz.pair Fuzz.int myNiceFloat) Fuzz.string
    in
    fuzzTest fuzzer differ "dictWithTupleKeys"


setTest : () -> Test
setTest () =
    let
        differ =
            Differ.set Differ.string

        fuzzer =
            setFuzzer Fuzz.string
    in
    fuzzTest fuzzer differ "set"


listTest : () -> Test
listTest () =
    let
        differ =
            Differ.list Differ.string

        fuzzer =
            Fuzz.list Fuzz.string
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
            Fuzz.map3 (\a b c -> { a = a, b = b, c = c })
                Fuzz.bool
                Fuzz.bool
                Fuzz.bool
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
            Fuzz.listOfLengthBetween 0
                16
                (Fuzz.map3 (\a b c -> { a = a, b = b, c = c })
                    (dictFuzzer Fuzz.int Fuzz.niceFloat)
                    (setFuzzer Fuzz.char)
                    (Fuzz.map (\x -> { x = x }) Fuzz.string)
                )
    in
    fuzzTest fuzzer differ "complex"


type Custom
    = A Int
    | B String


customTest : () -> Test
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
            Fuzz.oneOf
                [ Fuzz.map A Fuzz.int
                , Fuzz.map B Fuzz.string
                ]
    in
    fuzzTest fuzzer differ "custom"



-- Helpers


dictFuzzer : Fuzzer comparable -> Fuzzer v -> Fuzzer (Dict.Dict comparable v)
dictFuzzer k v =
    Fuzz.listOfLengthBetween 0 16 (Fuzz.pair k v)
        |> Fuzz.map Dict.fromList


setFuzzer : Fuzzer comparable -> Fuzzer (Set.Set comparable)
setFuzzer m =
    Fuzz.listOfLengthBetween 0 16 m
        |> Fuzz.map Set.fromList


myNiceFloat : Fuzzer Float
myNiceFloat =
    Fuzz.floatRange -10000000 10000000


fuzzTest : Fuzzer b -> Differ.Differ b -> String -> Test
fuzzTest fuzzer differ name =
    Test.fuzz2 fuzzer fuzzer name <|
        \old new ->
            let
                diff =
                    Differ.run differ old new
            in
            Differ.patch differ diff old
                |> Expect.equal (Ok new)
