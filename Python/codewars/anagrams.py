def anagrams(word, words):
    result = []
    for i in words:
        if len(word) is len(i):
            valid = True
            for n in i:
                if i.count(n) != word.count(n):
                    valid = False
                    break
            if valid:
                result.append(i)
    return result
