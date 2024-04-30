
tuple1 = ("Test", 12, ["Hello", "Fellow", "Human"])
pair1 = ("12345", "67890")   -- pairs are tuples of 2 items

-- Will only work with a pair (Tuple of 2 items)
first_from_tuple pair = fst pair

-- Will return '4' because strings are tuples
first_from_tuple' = fst pair1 !! 3

-- Will only work with a pair
second_from_tuple pair = snd pair

