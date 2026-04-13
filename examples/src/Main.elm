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
        , H.section []
            [ H.h3 [] [ H.text <| "output == new?" ]
            , H.text <| Debug.toString (output == new)
            ]
        ]


type Custom
    = A Int
    | B Bool


customDiffer =
    Differ.custom
        (\a b variant ->
            case variant of
                A arg ->
                    a arg

                B arg ->
                    b arg
        )
        |> Differ.variant1 A Differ.int
        |> Differ.variant1 B Differ.bool
        |> Differ.endCustom


type alias User =
    { name : String
    , isCool : { really : Bool }
    , prefers : Prefers
    }


type Prefers
    = Words (Set.Set String)
    | Numbers Int


old =
    [ ed
    , simon
    , jeroen
    , leonardo
    , mario
    ]


new =
    [ { ed | prefers = Words (Set.fromList [ "inconceivable!", "hello" ]) }
    , jeroen
    , mario
    , leonardo
    ]


ed : User
ed =
    { name = "Ed"
    , isCool = { really = False }
    , prefers = Words (Set.fromList [ "hello" ])
    }


simon : User
simon =
    { name = "Simon"
    , isCool = { really = True }
    , prefers = Words (Set.fromList [ "virtual", "dom" ])
    }


jeroen : User
jeroen =
    { name = "Jeroen"
    , isCool = { really = True }
    , prefers = Words (Set.fromList [ "elm-review" ])
    }


leonardo : User
leonardo =
    { name = "Leonardo"
    , isCool = { really = True }
    , prefers = Numbers 1
    }


mario : User
mario =
    { name = "Mario"
    , isCool = { really = True }
    , prefers = Words (Set.fromList [ "lamdera" ])
    }


userListDiffer =
    Differ.list userDiffer


userDiffer : Differ.Differ User
userDiffer =
    Differ.pure User
        |> Differ.andMap .name Differ.string
        |> Differ.andMap .isCool (Differ.map .really (\r -> { really = r }) Differ.bool)
        |> Differ.andMap .prefers
            (Differ.custom
                (\w n v ->
                    case v of
                        Words ws ->
                            w ws

                        Numbers ns ->
                            n ns
                )
                |> Differ.variant1 Words (Differ.set Differ.string)
                |> Differ.variant1 Numbers Differ.int
                |> Differ.endCustom
            )


myDiff : Differ.Changes (List User)
myDiff =
    Differ.run userListDiffer old new


output : List User
output =
    Differ.patch userListDiffer myDiff old
