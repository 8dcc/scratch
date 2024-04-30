{-
    Run with:
        $ ghci
        ghci> :l filename
        ghci> func_name param1 param2
-}

-- Simple function with 1 param.
double_me x = x + x

-- Simple function with 2 params.
double_us x y = x * 2 + y * 2

-- Simple function with conditional
double_with_conditional x =
    if x > 99
        then x
        else x * 2

-- Same as before, but enclosing the conditional so we can add to the final value 1
-- double_with_conditional' 123 == 124
-- double_with_conditional' 12  == (12 * 2) + 1 == 25
double_with_conditional' x = (if x > 99 then x else x * 2) + 1
