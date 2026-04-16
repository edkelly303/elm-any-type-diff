module Helpers exposing (bracket, chompOnly, floatParser, quoteString, quotedStringParser, unbracket)

import Parser exposing (..)


{-| In our case, we can't use `Parser.float` from elm/parser because it has a
bug with very large numbers - see <https://github.com/elm/parser/issues/58>.

This implementation is probably slower and won't handle things like

    > 1.79*10.0^308
    1.79e+308 : Float
    > String.fromFloat(1.79*10.0^308)
    "1.79e+308" : String

But that's ok in our case, because we don't need to handle Float literals, only
values produced by `String.fromFloat`.

-}
floatParser : Parser Float
floatParser =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= floatParserHelp
        , floatParserHelp
        ]


floatParserHelp : Parser Float
floatParserHelp =
    let
        oneOrMoreDigits =
            Parser.succeed ()
                |. Parser.chompIf Char.isDigit
                |. Parser.chompWhile Char.isDigit
    in
    Parser.succeed
        (\start { usesENotation } end source ->
            { chompedString = String.slice start end source
            , usesENotation = usesENotation
            }
        )
        |= Parser.getOffset
        |. oneOrMoreDigits
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.chompIf (\c -> c == '.')
                |. oneOrMoreDigits
                |= Parser.oneOf
                    [ -- it's a number like `1.01e+21`
                      Parser.succeed { usesENotation = True }
                        |. Parser.chompIf (\c -> c == 'e')
                        |. Parser.oneOf
                            [ Parser.chompIf (\c -> c == '+')
                            , Parser.chompIf (\c -> c == '-')
                            ]
                        |. oneOrMoreDigits

                    -- it's a number like `1.1`
                    , Parser.succeed { usesENotation = False }
                    ]

            -- it's a number like `1`
            , Parser.succeed { usesENotation = False }
            ]
        |= Parser.getOffset
        |= Parser.getSource
        |> Parser.andThen
            (\{ chompedString, usesENotation } ->
                if usesENotation then
                    -- bail out and use `Parser.float` instead
                    case Parser.run Parser.float chompedString of
                        Ok f ->
                            Parser.succeed f

                        Err _ ->
                            Parser.problem "Not a float"

                else
                    -- just use `String.toFloat`
                    case String.toFloat chompedString of
                        Just f ->
                            Parser.succeed f

                        Nothing ->
                            Parser.problem "Not a Float"
            )


{-| This function comes from
<https://github.com/myrho/elm-parser-extras/tree/1.0.1> but the implementation
there seems to have a bug with an unguarded `chompWhile` leading to infinite
looping. Fixed version is here.
-}
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
bracket opener closer str =
    opener ++ str ++ closer


unbracket : String -> String -> String -> String
unbracket opener closer str =
    str
        |> String.dropLeft (String.length opener)
        |> String.dropRight (String.length closer)


quoteString : String -> String
quoteString str =
    let
        quoted =
            str
                |> String.replace "/" "//"
                |> String.replace "<" "/<"
    in
    "<" ++ quoted ++ "<"
