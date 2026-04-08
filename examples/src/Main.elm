module Main exposing (main)

import Dict
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


u1 : Dict.Dict String User
u1 =
    Dict.fromList
        [ ( "Ed", { name = "Ed", isCool = { really = True } } )
        , ( "Simon", { name = "Simon", isCool = { really = True } } )
        , ( "Leonardo", { name = "Leonardo", isCool = { really = True } } )
        ]


u2 : Dict.Dict String User
u2 =
    Dict.fromList
        [ ( "Ed", { name = "Ed", isCool = { really = False } } )
        , ( "Mario", { name = "Mario", isCool = { really = True } } )
        , ( "Simon", { name = "Simon", isCool = { really = True } } )
        ]


userDictDiffer : Differ.Differ (Dict.Dict String User) (Dict.Dict String User)
userDictDiffer =
    Differ.dict Differ.string userDiffer


userDiffer : Differ.Differ User User
userDiffer =
    Differ.pure User
        |> Differ.andMap .name Differ.string
        |> Differ.andMap .isCool (Differ.map .really (\r -> { really = r }) Differ.bool)


myDiff : Differ.Diff (Dict.Dict String User)
myDiff =
    Differ.run userDictDiffer u1 u2


output : Maybe (Dict.Dict String User)
output =
    Differ.patch userDictDiffer myDiff u1
