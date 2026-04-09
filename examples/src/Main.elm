module Main exposing (main)

import Dict
import Differ
import Html as H
import Set


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
    , favouriteWords : Set.Set String
    }


u1 : Dict.Dict String User
u1 =
    mkUsers
        [ ed
        , simon
        , leonardo
        ]


u2 : Dict.Dict String User
u2 =
    mkUsers
        [ { ed | favouriteWords = Set.insert "oooh" ed.favouriteWords }
        , simon
        , mario
        ]


ed : User
ed =
    { name = "Ed"
    , isCool = { really = False }
    , favouriteWords = Set.fromList [ "hello" ]
    }


simon : User
simon =
    { name = "Simon"
    , isCool = { really = True }
    , favouriteWords = Set.fromList [ "world" ]
    }


leonardo : User
leonardo =
    { name = "Leonardo"
    , isCool = { really = True }
    , favouriteWords = Set.fromList [ "wow" ]
    }


mario : User
mario =
    { name = "Mario"
    , isCool = { really = True }
    , favouriteWords = Set.fromList [ "hurray" ]
    }


mkUsers : List User -> Dict.Dict String User
mkUsers list =
    list
        |> List.map (\u -> ( u.name, u ))
        |> Dict.fromList


userDictDiffer : Differ.Differ (Dict.Dict String User)
userDictDiffer =
    Differ.dict Differ.string userDiffer


userDiffer : Differ.Differ User
userDiffer =
    Differ.pure User
        |> Differ.andMap .name Differ.string
        |> Differ.andMap .isCool (Differ.map .really (\r -> { really = r }) Differ.bool)
        |> Differ.andMap .favouriteWords (Differ.set Differ.string)


myDiff : Differ.Diff (Dict.Dict String User)
myDiff =
    Differ.run userDictDiffer u1 u2


output : Dict.Dict String User
output =
    Differ.patch userDictDiffer myDiff u1
