-- Char
char = 'a'

-- [Char] : List of chars. We don't care about the ammount, but we care about all
-- items having the same type.
char_list = "Hello"

-- ([Char], Bool) : Tuple with 2 items (pair), the first one has a Char list and the
-- seccond item has a Bool. Remember that with tuples, we don't care about all items
-- having the same type, but we care about how many items we have.
char_tupl = ("Hello", True)

-- Bool : In this case false
bool = 4 == 5

-- We can declare the type of our functions with "::". For example,
-- remove_non_uppercase takes a string (List of Chars) and returns another string
-- (Another list of Chars):
remove_non_uppercase :: [Char] -> [Char]
remove_non_uppercase s = [c | c <- s, c `elem` ['A' .. 'Z']]

-- In this case it is not really necesary because the compiler knows what we want to
-- do, but it's still a good practise.

-- Writing "[Char]" as a type is the same as writing "String". So we could do:
remove_non_uppercase' :: String -> String
remove_non_uppercase' s = [c | c <- s, c `elem` ['A' .. 'Z']]

-- If we have more parameters, we would use the same division as the return type.
-- func      arg1   arg2   arg3   returned (Last one so the rest are args)
add_three :: Int -> Int -> Int -> Int
add_three a b c = a + b + c

-- If you are not sure about the type a function returns, you can use ":t funcname"
-- in ghci.
unknown_type x = [n * 2 | n <- x, n < 20]

-- Remember that Int is the usual integer (-2147483648 to 2147483647), but Integer
-- can store *really* big numbers.
-- Float is single precision floating point and double is double precision floating
-- point.
-- Bool can only be True or False.
-- Char is a character and [Char] is the same as String.

