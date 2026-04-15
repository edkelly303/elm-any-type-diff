module Helpers exposing (bracket, chompOnly, quoteString, quotedStringParser, unbracket)

import Parser exposing (..)


quotedStringParser : Char -> Char -> Parser String
quotedStringParser escape quote =
    succeed identity
        |. chompIf ((==) quote)
        |= loop "" (quotedStringParserHelp escape quote)


quotedStringParserHelp : Char -> Char -> String -> Parser (Step String String)
quotedStringParserHelp escape quote string =
    oneOf
        [ token (String.fromList [ escape, quote ])
            |> map (\_ -> string ++ String.fromChar quote |> Loop)
        , token (String.fromList [ escape, escape ])
            |> map (\_ -> string ++ String.fromChar escape |> Loop)
        , chompIf ((==) quote)
            |> map (\_ -> Done string)
        , chompIf ((==) escape)
            |> map (\_ -> string ++ String.fromChar escape |> Loop)
        , succeed ()
            |. chompIf (\c -> c /= quote && c /= escape)
            |. chompWhile (\c -> c /= quote && c /= escape)
            |> getChompedString
            |> map (\s -> string ++ s |> Loop)
        ]


chompOnly : Char -> Parser ()
chompOnly oneChar =
    Parser.chompIf (\thisChar -> thisChar == oneChar)


bracket : String -> String -> String -> String
bracket pre post str =
    pre ++ " " ++ str ++ " " ++ post


unbracket : Int -> Int -> String -> String
unbracket atStart atEnd str =
    str
        |> String.dropLeft atStart
        |> String.dropRight atEnd


quoteString : String -> String
quoteString str =
    let
        quoted =
            str
                |> String.replace "/" "//"
                |> String.replace "<" "/<"
    in
    "<" ++ quoted ++ "<"
