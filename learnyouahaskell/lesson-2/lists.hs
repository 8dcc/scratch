
list1 = ("Test", 12, ["Hello", "Fellow", "Human"])
pair1 = ("12345", "67890")   -- pairs are lists of 2 items

-- Will only work with a pair (List of 2 items)
first_from_list pair = fst pair

-- Will return '4' because strings are tuples
first_from_list' = fst pair1 !! 3

-- Will only work with a pair
second_from_list pair = snd pair

