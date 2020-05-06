module CasePractice where

-- exercise 1
functionC x y = if (x > y) then x else y

functionC' x y =
    case x > y of
        True -> x
        False -> y

-- exercise 2
ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' n =
    case even n of
        True -> n + 2
        False -> n


-- exercise 3
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0
