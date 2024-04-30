-- https://gist.github.com/r4v10l1/3051b68d9d133e1b2bd95cabd61b850c

list1 = [[1, 2, 3, 4], [4, 3, 2, 1]]

-- Access lists
first_of_list list = head list
last_of_list list = last list
remove_element_from_top list = tail list    -- list[1:]
remove_element_from_bottom list = init list -- list[:-1]

-- Return first n values from list
first_vals n list = take n list             -- list[:n]

-- Remove first n values from list. n is the ammount you want to remove from the
-- top, not the values you want to get from the bottom.
last_vals n list = drop n list              -- list[n:]

-- Maximum and minimum vals from list
minimumValue list = minimum list
maximumValue list = maximum list

-- Sum and mult lists
add_all_vals list = sum list                -- for (i in list) ret += i
mult_all_vals list = product list           -- for (i in list) ret *= i

-- Check if item is in list
is_in_list x list = x `elem` list           -- Returns True if x in list

-- Returns a list of "count" items filled with x. We are "generating an infinite
-- list" with repeat, and taking the items we want. Doesn't work with lists.
repeat_element x count = take count (repeat x)

-- Returns n elements of looped list. We are "generating an infinite list"
-- contaning the items of list looped, and taking the items we want. Works with
-- lists. Example:
--   looped_list 7 [1, 2, 3] == [1, 2, 3, 1, 2, 3, 1]
looped_list count list = take count (cycle list)

-- Access idx of list
access_idx idx list = list !! idx           -- list[idx]

-- Get length and check if list is empty
get_length list = length list
is_empty list = null list

is_empty' list =
    if null list
        then "It is empty!"
        else "It is not empty!"

-- Reverse list
get_reversed list = reverse list

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
-- each value of list to nowhere, because we only care about *how many times* we are
-- doing that.
length' list = sum [1 | _ <- list]

-- Will return c. c is the list only if c is in ['A'..'Z']. Would be:
--   ret = "";
--   for (c in str)
--     if (c not in ['A'..'Z'])
--        ret += c;
remove_non_upper str = [c | c <- str, c `elem` ['A' .. 'Z']]

-- For each value in "chars", it will check if it is in "target". It returns a list
-- of True's and False's for each check. We dont need to add a filter with ',' 
-- (predicate) like with other funcs, because we want all values converted.
find_chars chars target = [if c `elem` target then True else False | c <- chars]

