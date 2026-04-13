module Main exposing (main)

import Dict
import Differ
import Html as H
import Set


main : H.Html msg
main =
    H.main_ []
        [ H.section []
            [ H.h3 [] [ H.text <| "old" ]
            , H.text <| Debug.toString old
            ]
        , H.section []
            [ H.h3 [] [ H.text <| "new" ]
            , H.text <| Debug.toString new
            ]
        , H.section []
            [ H.h3 [] [ H.text <| "diff" ]
            , H.text <| Debug.toString myDiff
            ]
        , H.section []
            [ H.h3 [] [ H.text <| "output" ]
            , H.text <| Debug.toString output
            ]
        ,    H.section []
            [ H.h3 [] [ H.text <| "output == new?" ]
            , H.text <| Debug.toString (output == new)
            ]
        ]


type alias User =
    { name : String
    , isCool : { really : Bool }
    , favouriteWords : Set.Set String
    }


old =
    [ ed
    , simon
    , simon
    , simon
    , leonardo
    , leonardo
    , leonardo
    ]


new =
    [ { ed | favouriteWords = Set.insert "oooh" ed.favouriteWords }
    , simon
    , simon
    , simon
    , mario
    , leonardo
    , leonardo
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


userListDiffer =
    Differ.list userDiffer


userDiffer : Differ.Differ User
userDiffer =
    Differ.pure User
        |> Differ.andMap .name Differ.string
        |> Differ.andMap .isCool (Differ.map .really (\r -> { really = r }) Differ.bool)
        |> Differ.andMap .favouriteWords (Differ.set Differ.string)


myDiff : Differ.Changes (List User)
myDiff =
    Differ.run userListDiffer old new


output : List User
output =
    Differ.patch userListDiffer myDiff old
