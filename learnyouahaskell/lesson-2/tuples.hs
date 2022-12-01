-- https://gist.github.com/r4v10l1/3051b68d9d133e1b2bd95cabd61b850c

tuple1 = [[1, 2, 3, 4], [4, 3, 2, 1]]

-- Access tuples
first_of_tuple tupl = head tupl
last_of_tuple tupl = last tupl
remove_element_from_top tupl = tail tupl    -- tupl[1:]
remove_element_from_bottom tupl = init tupl -- tupl[:-1]

-- Return first n values from tuple
first_vals n tupl = take n tupl             -- tupl[:n]

-- Remove first n values from tuple. n is the ammount you want to remove from the
-- top, not the values you want to get from the bottom.
last_vals n tupl = drop n tupl              -- tupl[n:]

-- Maximum and minimum vals from tuple
minimumValue tupl = minimum tupl
maximumValue tupl = maximum tupl

-- Sum and mult tuples
add_all_vals tupl = sum tupl                -- for (i in tupl) ret += i
mult_all_vals tupl = product tupl           -- for (i in tupl) ret *= i

-- Check if item is in tuple
is_in_tupl x tupl = x `elem` tupl           -- Returns True if x in tupl

-- Returns a tuple of "count" items filled with x. We are "generating an infinite
-- tuple" with repeat, and taking the items we want. Doesn't work with lists.
repeat_element x count = take count (repeat x)

-- Returns n elements of looped tupl. We are "generating an infinite tuple"
-- contaning the items of tupl looped, and taking the items we want. Works with
-- lists. Example:
--   looped_list 7 [1, 2, 3] == [1, 2, 3, 1, 2, 3, 1]
looped_list count tupl = take count (cycle tupl)

-- Access idx of tuple
access_idx idx tupl = tupl !! idx           -- tupl[idx]

-- Get length and check if tupl is empty
get_length tupl = length tupl
is_empty tupl = null tupl

is_empty' tupl =
    if null tupl
        then "It is empty!"
        else "It is not empty!"

-- Reverse tuple
get_reversed tupl = reverse tupl

------------------------------------------------------------------------------------

-- Get a list of numbers from a to b. Multiply by 2. Check if the remainder when
-- divided with 7 is 0. Would be:
--   x = [a .. b];
--   x *= 2;
--   return (x % 7 == 0);
-- Stuff after the comma is a condition (called predicate)
range_func n1 n2 = [x * 2 | x <- [n1 .. n2], rem x 7 == 0]

-- Get pair numbers from range
pairs1 n1 n2 = [rem x 2 == 0 | x <- [n1 .. n2]]     -- Stores true or false for all items
pairs2 n1 n2 = [x | x <- [n1 .. n2], rem x 2 == 0]  -- Stores only pairs

-- Replaces each element with 1 and then sums it. '_' here means that we are sending
-- each value of tupl to nowhere, because we only care about *how many times* we are
-- doing that.
length' tupl = sum [1 | _ <- tupl]

-- Will return c. c is the list only if c is in ['A'..'Z']. Would be:
--   ret = "";
--   for (c in str)
--     if (c not in ['A'..'Z'])
--        ret += c;
remove_lower str = [c | c <- str, c `elem` ['A' .. 'Z']]

-- For each value in "chars", it will check if it is in "target". It returns a list
-- of True's and False's for each check. We dont need to add a filter with ',' 
-- (predicate) like with other funcs, because we want all values converted.
find_chars chars target = [if c `elem` target then True else False | c <- chars]

