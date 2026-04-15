# Any type diff

## What?

A little package that helps to reduce the amount of data you need to send over
the network when your Elm values change. Potentially useful if you're using
Lamdera or some similar full-stack Elm framework that sends data between backend
and frontend.

## Why?

I wrote this out of curiosity, not necessity, and it's almost certainly not the
"right" way to produce minimal diffs efficiently. I am sure the many years of
research into diffing algorithms by professional computer scientists will have
resulted in something all-around better than my naive approach. But I tried and
failed to understand Myers' algorithm, so this is what I came up with instead.

## How?

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

-- But what happens if we accidentally mix up our users 
-- and apply the `Delta` to the wrong person's record?

wrongUser =
    { name = "Arthur"
    , age = 2
    , hobbies = [ "gardening", "playing with toys" ]
    }

uhoh =
    Differ.patch userDiffer delta wrongUser

-- Fortunately, this package has got it covered! The 
-- `Delta` includes a hash of the source value, and 
-- if the target value doesn't have a matching hash,
-- we just return an `Err`.

uhoh --> Err Differ.MismatchedDelta
```

## What's next?

- Serialize `Delta` values as `Bytes`, `Json`, etc.
- Add `variant0`, `variant2`, `variant3` etc.
- Figure out how to write `Differs` for recursive types.