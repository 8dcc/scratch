def comfortable_word(word):
    left_list = ["q", "w", "e", "r", "t", "a", "s", "d", "f", "g", "z", "x", "c", "v", "b"]
    right_list = ["y", "u", "i", "o", "p", "h", "j", "k", "l", "n", "m"]
    last = ""
    for i in word:
        current = "right" if i in left_list else "left"
        if last == current:
            return False
        last = "right" if i in left_list else "left"
    return True
