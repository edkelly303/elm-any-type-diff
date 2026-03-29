module Main exposing (main)

import Differ
import Html as H


main : H.Html msg
main =
    H.main_ []
        [ H.section []
            [ H.h3 [] [ H.text <| "input (old)" ]
            , H.text <| Debug.toString u1
            ]
        , H.section []
            [ H.h3 [] [ H.text <| "input (new)" ]
            , H.text <| Debug.toString u2
            ]
        , H.section []
            [ H.h3 [] [ H.text <| "diff" ]
            , H.text <| Debug.toString myDiff
            ]
        , H.section []
            [ H.h3 [] [ H.text <| "output" ]
            , H.text <| Debug.toString output
            ]
        ]


type alias User =
    { name : String
    , isCool : { really : Bool }
    }


u1 : User
u1 =
    { name = "Ed", isCool = { really = True } }


u2 : User
u2 =
    { name = "Ed", isCool = { really = False } }


userDiffer : Differ.Differ { a | isCool : { really : Bool }, name : String } User
userDiffer =
    Differ.pure User
        |> Differ.andMap .name Differ.string
        |> Differ.andMap .isCool (Differ.map .really (\r -> { really = r }) Differ.bool)


myDiff : Differ.Diff { isCool : { really : Bool }, name : String }
myDiff =
    Differ.run userDiffer u1 u2


output : Maybe User
output =
    Differ.patch userDiffer myDiff u1
