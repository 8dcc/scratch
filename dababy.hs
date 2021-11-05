-- https://gist.github.com/r4v10l1/3051b68d9d133e1b2bd95cabd61b850c

doubleMe x = x + x
doubleUs x y = x*2 + y*2

doubleIf2Digits x = if x > 99
                        then x
                        else x*2

doubleIf2Digits' x = (if x > 99 then x else x*2) + 1

--------------------------------------------------------------------------------
-- LISTS (actually tuples but its ok)

imatuple = [[1, 2, 3, 4], [4, 3, 2, 1]]

firstElementOfAList x = head x
lastElementOfAList x = last x
removeFirstElementOfAList x = tail x
removeLastElementOfAList x = init x

-- Remove a custom ammount from the end
customInit x y = take x y
-- Remove a custom ammount from the begining
customTail x y = drop x y

minimumValue x = minimum x
maximumValue x = maximum x

addAllValuesFromList x = sum x
multiplyAllValuesFromList x = product x

checkIfInList x y = x `elem` y  -- Returns True if x in y

onlyFirstCharacters x y = take x y  -- Returns the first x characters from y
repeatElementXAmmount x y = take x (repeat y)  -- Won't work with lists
repeatListXAmmount x y = take x (cycle y)  -- Will work with lists

extractSingleValue x y = y !! x  -- Will extract x index. In python y[x]

getLengthOfList x = length x
checkIfListIsEmpty x = null x

checkIfListIsEmpty' x = if null x
                    then "Yee"
                    else "Naah"

reverseList x = reverse x

--------------------------------------------------------------------------------

-- Get a list of numbers from a to b. Multiply by 2. Check if the remainder when divided with 7 is 0
bitchThing a b = [x*2 | x <- [a..b], rem x 7 == 0]

--------------------------------------------------------------------------------

length' x = sum [1 | _ <- x]  -- Replaces each element with 1 and then sums it.

-- Will return c. c is the list only if c is in ['A'..'Z']
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- For each value in x, it will check if it is in y. (I think it's like a for loop idk)
-- It returns a list of True's and False's for each check.
-- We dont need to add a filter with <,> like in the previous func, because we want all values converted.
elemWithLists x y = [ if c `elem` y then True else False | c <- x]

--------------------------------------------------------------------------------
-- LISTS NOW FR

imalistfr = ("Bruh", 12, ["Suck", "My", "Dick"])
imashortlist = ("First item", "Second item")

firstItemFromAList x = fst x  -- Will only work with a pair (List of 2 items)
firstItemFromAList' = fst imashortlist !! 3  -- Will return 's' because strings are tuples
secondItemFromAList x = snd x  -- Will only work with a pair
