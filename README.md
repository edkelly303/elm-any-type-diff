# Any type diff

```elm
import Differ exposing (Differ)

-- Let's say we want to calculate a diff between 
-- these two `User` records:

type alias User = 
    { name : String
    , age : Int
    , hobbies : List String 
    }

oldUser = 
    { name = "Ed"
    , age = 44
    , hobbies = [ "programming", "reading" ]
    }

newUser = 
    { name = "Ed"
    , age = 44
    , hobbies = [ "programming", "reading", "cricket" ]
    }

-- First, we define a `Differ` for the `User` type:

userDiffer : Differ User
userDiffer =
    Differ.pure User
        |> Differ.andMap .name Differ.string
        |> Differ.andMap .age Differ.int
        |> Differ.andMap .hobbies (Differ.list Differ.string)

-- Then we generate a `Delta`:

delta = 
    Differ.run userDiffer oldUser newUser

-- And now we can use the `Delta` to patch 
-- the old user to match the new one:

patchedUser = 
    Differ.patch userDiffer delta oldUser

-- And confirm that the patched user is identical 
-- to `newUser`:

patchedUser == Ok newUser --> True
```
