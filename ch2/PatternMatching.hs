module PatternMatching where

customGreeting "George" = "Oh, hey George"
customGreeting name = "Hello, " <> name

addValues [] = 0
addValues (first:rest) = first + addValues(rest)

favoriteFood person =
  case person of
    "Ren"     -> "Tofu"
    "Rebecca" -> "Falafel"
    "George"  -> "Bannana"
    name      -> "I Dont Know"

handleNums l =
  case l of
    [] -> "An Empty List"
    [x] | x == 0 -> "Zero"
        | x == 1 -> "One"
        | even x -> "Even"
        | otherwise -> "Unhandled"
    _list -> "The list has more than one element"
