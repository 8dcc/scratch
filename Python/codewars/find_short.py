def find_short(s):
    result = len(s.split(" ")[0])
    for word in s.split(" "):
        if len(word) <= result:
            result = len(word)
    return result
